{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util
import Data.List
import Data.Traversable
import Text.Printf

-- preEddy
--path=${StudyFolder} \ - Path to subject's data folder
--subject=${Subject} \
--dwiname=${DWIName} \  name to give DWI output directories
--PEdir=${PEdir} \ 1=RL/LR, 2=PA/AP
--posData=${PosInputImages} \ [Img]
--negData=${NegInputImages} \ [Img]
--echospacing=${echospacing} \ in msecs
--b0maxbval=${b0maxbval} \Volumes with a bvalue smaller than this value will be considered as b0s

-- newtype DWI'Pos = DWI'Pos DWI
-- newtype DWI'Neg = DWI'Neg DWI
-- (DWI'Pos,DWI'Neg)
-- phaseEncodingPA = PhaseEncoding { pos = PA, neg = AP }

-- ${runcmd} cp ${absname}.bval ${outdir}/rawdata/${basePos}_${Pos_count}.bval
-- PA_1.{nii.gz,bval,bvec}
-- AP_1.{nii.gz,bval,bvec}

pas :: [FilePath]
pas = ["BIO_0001.dwiPA1.nii.gz"]

aps :: [FilePath]
aps = ["BIO_0001.dwiAP1.nii.gz"]

pa_bvals = ["BIO_0001.dwiPA1.bval"]
pa_bvecs = ["BIO_0001.dwiPA1.bvec"]
ap_bvals = ["BIO_0001.dwiAP1.bval"]
ap_bvecs = ["BIO_0001.dwiAP1.bvec"]

b0maxbval = 45


trim = unwords . words

numDirs :: FilePath -> Action Int
numDirs dwi = read . trim . fromStdout <$> command [] "fslval" [dwi, "dim4"]

data DWI = PE1 { filepath :: FilePath } | PE2 { filepath:: FilePath }
  deriving Show

phaseLength :: DWI -> Action PhaseLength
phaseLength (PE1 dwi) = read . fromStdout <$> command [] "fslval" [dwi, "dim1"]
phaseLength (PE2 dwi) = read . fromStdout <$> command [] "fslval" [dwi, "dim2"]

type EchoSpacing = Float
type PhaseLength = Int

readoutTime :: PhaseLength -> EchoSpacing -> Float
readoutTime l echo = (echo * numPEsteps) / 1000
  where numPEsteps = fromIntegral $ l - 1

toCSV :: Show a => [[a]] -> String
toCSV d = intercalate "\n" (intercalate "," . map show <$> d)

extractVol :: DWI -> Int -> Action FilePath
extractVol dwi idx
  = outPath <$ (cmd :: Action ())
    where
      cmd = command [] "fslroi" [filepath dwi, outPath, show idx, show 1]
      outPath = replaceExtensions (filepath dwi) (printf "b0-%04d.nii.gz" idx)

readArrayFile :: FilePath -> Action [Int]
readArrayFile bval = map read <$> words <$> readFile' bval

extractB0s :: DWI -> [Int] -> Int -> Action [FilePath]
extractB0s dwi bValues b0max =
  traverse (extractVol dwi) b0indices
  where
    b0indices = findIndices (< b0max) bValues

mergeB0s :: FilePath -> [FilePath] -> Action ()
mergeB0s out b0s = unit $ command [] "fslmerge" (["-t", out] ++ b0s)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build"} $ do
    want ["build/001/eddy/SeriesVolNum.csv"]
    want ["build/topup/Pos_b0.nii.gz"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "_build" ["//*"]

    "build//SeriesVolNum.csv" %> \out -> do
      need aps
      need pas
      ds <- traverse numDirs aps
      ds' <- traverse numDirs pas
      let csv = toCSV $ zipWith (\x y -> [min x y, x, y]) ds ds'
      case csv of
        [] -> error "No pairs of phase encoding directions have been found, at least one is needed"
        _ -> writeFile' out csv

    "build/topup/Pos_b0.nii.gz" %> \out -> do
      need $ pa_bvals ++ pas
      let pa = head pas
      let bval = head pa_bvals
      bValues <- readArrayFile bval
      putNormal $ show (findIndices (< b0maxbval) bValues)
      b0s <- extractB0s (PE1 pa) bValues b0maxbval
      mergeB0s out b0s




-- b0dist=45 #Minimum distance in volums between b0s considered for preprocessing
-- ${runcmd} ${HCPPIPEDIR_dMRI}/basic_preproc.sh ${outdir} ${echospacing} ${PEdir} ${b0dist} ${b0maxbval}
