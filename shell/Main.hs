{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
import Control.Monad

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

data Dir = Dir Double Double Double

instance Show Dir where
  show (Dir v1 v2 v3) = printf "%f %f %f" v1 v2 v3

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

replaceExtension' :: FilePath -> String -> FilePath
replaceExtension' f ext = replaceExtension (dropExtension f) ext

extractVol :: DWI -> Int -> Action FilePath
extractVol dwi idx
  = outPath <$ (cmd :: Action ())
    where
      cmd = command [] "fslroi" [filepath dwi, outPath, show idx, show 1]
      outPath = replaceExtension' (filepath dwi) (printf "b0-%04d.nii.gz" idx)

readbval :: FilePath -> Action [Int]
readbval f = map read <$> words <$> readFile' f

writebval :: FilePath -> [Int] -> Action ()
writebval out arr = writeFile' out (unwords . map show $ arr)

extractB0s :: DWI -> [Int] -> Int -> Action [FilePath]
extractB0s dwi bValues b0max =
  traverse (extractVol dwi) b0indices
  where
    b0indices = findIndices (< b0max) bValues

mergeVols :: FilePath -> [FilePath] -> Action ()
mergeVols out vols = unit $ command [] "fslmerge" (["-t", out] ++ vols)

tobval :: FilePath -> FilePath
tobval f = replaceExtension' f "bval"

tobvec :: FilePath -> FilePath
tobvec f = replaceExtension' f "bvec"

readDWIBval :: DWI -> Action [Int]
readDWIBval dwi = readbval (replaceExtension' f "bval")
  where f = filepath dwi

-- readDWIBvec :: DWI -> Action [Dir]
-- readDWIBvec dwi = readbvec (replaceExtension' f "bvec")
--   where f = filepath dwi

-- readArrayFiles :: Read a => [FilePath] -> Action [a]
-- readArrayFiles fs = concat <$> (traverse readArrayFile fs)

mergebvals :: FilePath -> [FilePath] -> Action ()
mergebvals out = readbvals >=> writebval out
  where
    readbvals fs = concat <$> (traverse readbval fs)

mergebvecs :: FilePath -> [FilePath] -> Action ()
mergebvecs out = readbvecs >=> writebvec out
  where
    readbvecs fs = concat <$> (traverse readbvec fs)

readbvec :: FilePath ->  Action [Dir]
readbvec f = toVecs <$> (map toArr) <$> readFileLines f
  where
    toVecs [v1,v2,v3] = (zipWith3 Dir) v1 v2 v3
    toArr = map read . words

writebvec :: FilePath -> [Dir] -> Action ()
writebvec f dirs = writeFile' f $ intercalate "\n" $ map show dirs

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build"} $ do
    want ["build/001/eddy/SeriesVolNum.csv"]
    want ["build/topup/Pos_Neg.nii.gz"]

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

    "build/topup/Pos_Neg.nii.gz" %> \out -> do
      let vols =  ["build/topup/Pos.nii.gz", "build/topup/Neg.nii.gz"]
      let b0s =  ["build/topup/Pos_b0.nii.gz", "build/topup/Neg_b0.nii.gz"]
      let bvals = map tobval vols
      let bvecs = map tobvec vols
      need $ vols ++ b0s ++ bvals ++ bvecs
      mergeVols out vols
      mergeVols (replaceExtension' out "_b0.nii.gz") b0s
      mergebvals (tobval out) bvals
      mergebvecs (tobvec out) bvecs

    ["build/topup/Neg.nii.gz",
     "build/topup/Neg.bval",
     "build/topup/Neg.bvec"] *>> \[outvol, outbval, outbvec] -> do
      need $ aps ++ ap_bvals ++ ap_bvecs
      mergebvals outbval ap_bvals
      mergebvecs outbvec ap_bvecs
      mergeVols outvol aps

    ["build/topup/Pos.nii.gz",
     "build/topup/Pos.bval",
     "build/topup/Pos.bvec"] *>> \[outvol, outbval, outbvec] -> do
      need $ pas ++ pa_bvals ++ pa_bvecs
      mergebvals outbval pa_bvals
      mergebvecs outbvec pa_bvecs
      mergeVols outvol pas

    "build/topup/Pos_b0.nii.gz" %> \out -> do
      need $ pa_bvals ++ pas
      let pa = head pas
      let bval = head pa_bvals
      bValues <- readbval bval
      putNormal $ "Found b0's at indices: " ++ show (findIndices (< b0maxbval) bValues)
      b0s <- extractB0s (PE1 pa) bValues b0maxbval
      mergeVols out b0s

    "build/topup/Neg_b0.nii.gz" %> \out -> do
      need $ ap_bvals ++ aps
      let ap = head aps
      let bval = head ap_bvals
      bValues <- readbval bval
      putNormal $ "Found b0's at indices: " ++ show (findIndices (< b0maxbval) bValues)
      b0s <- extractB0s (PE1 ap) bValues b0maxbval
      mergeVols out b0s



-- b0dist=45 #Minimum distance in volums between b0s considered for preprocessing
-- ${runcmd} ${HCPPIPEDIR_dMRI}/basic_preproc.sh ${outdir} ${echospacing} ${PEdir} ${b0dist} ${b0maxbval}
