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

ap = "BIO_0001.dwiAP1.nii.gz"

trim = unwords . words

numDirs :: FilePath -> Action String
numDirs dwi = trim . fromStdout <$> command [EchoStdout True] "fslval" [dwi, "dim4"]

toCSV :: [[String]] -> String
toCSV d = intercalate "\n" $ intercalate "," <$> d


main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build"} $ do
    want ["build/001/eddy/SeriesVolNum.csv"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "_build" ["//*"]

    "build//SeriesVolNum.csv" %> \out -> do
      need aps
      need pas
      ds <- mapM numDirs aps
      ds' <- mapM numDirs pas
      let csv = toCSV $ zipWith (\x y -> [min x y, x , y]) ds ds'
      -- ds <- numDirs $ head aps
      -- cmd "mkdir " ++ (takeDirectory out)
      writeFile' out csv
