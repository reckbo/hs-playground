{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import           Control.Monad
import           Data.List
import           Data.Traversable
import           Development.Shake
import           Development.Shake.Command
import           Development.Shake.FilePath
import           Development.Shake.Util
import           Text.Printf

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
bvec = "BIO_0001.dwiAP1.bvec"

data PhaseDirection = RL | PA

phaseDirection = PA
b0maxbval = 45

trim = unwords . words

fslval :: String -> FilePath -> Action String
fslval key dwi = trim . fromStdout <$> command [] "fslval" [dwi, key]

numDirs :: FilePath -> Action Int
numDirs dwi = read <$> fslval "dim4" dwi

getDim3 :: FilePath -> Action Int
getDim3 = fmap read . fslval "dim3"

data Dir = Dir { v1::Double,
                 v2::Double,
                 v3::Double }

instance Show Dir where
  show (Dir v1 v2 v3) = printf "%f %f %f" v1 v2 v3

phaseLength :: FilePath -> Action PhaseLength
phaseLength dwi = case phaseDirection of
  PA -> read . fromStdout <$> command [] "fslval" [dwi, "dim1"]
  _   -> read . fromStdout <$> command [] "fslval" [dwi, "dim2"]

type EchoSpacing = Float
type PhaseLength = Int

readoutTime :: PhaseLength -> EchoSpacing -> Float
readoutTime l echo = (echo * numPEsteps) / 1000
  where numPEsteps = fromIntegral $ l - 1

toCSV :: Show a => [[a]] -> String
toCSV d = intercalate "\n" (intercalate "," . map show <$> d)

replaceExtension' :: FilePath -> String -> FilePath
replaceExtension' f ext = replaceExtension (dropExtension f) ext

extractVol :: FilePath -> Int -> Action FilePath
extractVol dwi idx
  = outPath <$ (cmd :: Action ())
    where
      cmd = command [] "fslroi" [dwi, outPath, show idx, show 1]
      outPath = replaceExtension' dwi (printf "b0-%04d.nii.gz" idx)

readbval :: FilePath -> Action [Int]
readbval f = map read <$> words <$> readFile' f

writebval :: FilePath -> [Int] -> Action ()
writebval out arr = writeFile' out (unwords . map show $ arr)

extractB0s :: FilePath -> [Int] -> Int -> Action [FilePath]
extractB0s dwi bValues b0max =
  traverse (extractVol dwi) b0indices
  where
    b0indices = findIndices (< b0max) bValues

extractVols' :: FilePath -> [Int] -> Action FilePath
extractVols' dwi idx
  = do
    fs <- traverse (extractVol dwi) idx
    mergeVols out fs
    return out
  where
    out = replaceExtension' dwi (printf "%s.nii.gz" (intercalate "-" $ map show idx))

mergeVols :: FilePath -> [FilePath] -> Action ()
mergeVols out vols = unit $ command [] "fslmerge" (["-t", out] ++ vols)

tobval :: FilePath -> FilePath
tobval f = replaceExtension' f "bval"

tobvec :: FilePath -> FilePath
tobvec f = replaceExtension' f "bvec"

readDWIBval :: FilePath -> Action [Int]
readDWIBval dwi = readbval (replaceExtension' dwi "bval")

-- readDWIBvec :: DWI -> Action [Dir]
-- readDWIBvec dwi = readbvec (replaceExtension' f "bvec")
--   where f = filepath dwi


readbvals :: [FilePath] -> Action [Int]
readbvals fs = concat <$> traverse readbval fs

mergebvals :: FilePath -> [FilePath] -> Action ()
mergebvals out = readbvals >=> writebval out

mergebvecs :: FilePath -> [FilePath] -> Action ()
mergebvecs outbvec fs = do
                  dirs <- readbvecs fs
                  case dirs of
                    Right xs -> writebvec outbvec xs
                    Left msg -> error msg
                where
                  readbvecs :: [FilePath] -> Action (Either String [Dir])
                  readbvecs fs = (fmap concat . sequenceA) <$> traverse readbvec fs

readbvec :: FilePath ->  Action (Either String [Dir])
readbvec f = toVecs <$> map toArr <$> readFileLines f
  where
    toVecs [v1,v2,v3] = Right $ zipWith3 Dir v1 v2 v3
    toVecs _ = Left $ "Seems to be an invalid bvecs file: " ++ f
    toArr = map read . words

writebvec :: FilePath -> [Dir] -> Action ()
writebvec f dirs = writeFile' f $ intercalate "\n" [v1',v2',v3']
  where v1' = unwords . map (show . v1) $ dirs
        v2' = unwords . map (show . v2) $ dirs
        v3' = unwords . map (show . v3) $ dirs

trimVol :: FilePath -> Action ()
trimVol dwi = do
  dim3 <- getDim3 dwi
  when (odd dim3) $ trimVol' dwi
  where
    trimVol' dwi = withTempFile $ \tmpfile -> do
      putNormal "DWI's have odd number of z-slices, remove one to make even"
      copyFile' dwi tmpfile
      command [] "fslroi" $ [tmpfile,dwi] ++ map show [0,-1,0,-1,1,-1]

combineSeriesRule :: String -> [FilePath] -> Rules ()
combineSeriesRule peDir dwis =
  let
    outvol = printf "build/topup/%s.nii.gz" peDir
    outbval = tobval outvol
    outbvec = tobvec outvol
    bvals = map tobval dwis
    bvecs = map tobvec dwis
  in
    [outvol,outbval,outbvec] &%> \_ -> do
      need $ dwis ++ bvals ++ bvecs
      mergebvals outbval bvals
      mergebvecs outbvec bvecs
      mergeVols outvol dwis

extractB0sRule :: String -> [FilePath] -> Rules ()
extractB0sRule peDir dwis
  = let
      outvol = printf "build/topup/%s_b0.nii.gz" peDir
      bvals = map tobval dwis
    in
      outvol %> \_ -> do
        need $ dwis ++ bvals
        bValues <- readbvals bvals
        putNormal $ "Found b0's at indices: " ++ show (findIndices (< b0maxbval) bValues)
        b0s <- concat <$> traverse (\f -> extractB0s f bValues b0maxbval) dwis
        mergeVols outvol b0s

-- data PosNegPair = PosNegPair
type BVal = FilePath
type DWI = FilePath
type Bvalue = Int
acqParamsPos = "0 1 0 0.8"
acqParamsNeg = "0 -1 0 0.8"

mkDWIPair :: DWI -> DWI -> [Bvalue] -> [Bvalue] -> DWIPair
mkDWIPair dwi dwi' bs bs'  = DWIPair dwi dwi' bs bs' idx idx' p p'
  where
    paired = zip bs bs'
    getB0Indices = findIndices (< b0maxbval)
    idx = getB0Indices . map fst $ paired
    idx' = getB0Indices . map snd $ paired
    p = replicate (length idx) acqParamsPos
    p' = replicate (length idx') acqParamsNeg

data DWIPair = DWIPair
  { dwi        :: FilePath
  , dwi'       :: FilePath
  , b0s        :: [Int]
  , b0s'       :: [Int]
  , idx        :: [Int]
  , idx'       :: [Int]
  , acqparams  :: [String]
  , acqparams' :: [String]
  } deriving Show

writePosB0s :: FilePath -> [DWIPair] -> Action ()
writePosB0s out dwipairs =
  do fs <- traverse writePosB0 dwipairs
     mergeVols out fs
  where writePosB0 dwipair = extractVols' (dwi dwipair) (idx dwipair)

writeNegB0s :: FilePath -> [DWIPair] -> Action ()
writeNegB0s out dwipairs =
  do fs <- traverse writeNegB0 dwipairs
     mergeVols out fs
  where writeNegB0 dwipair = extractVols' (dwi' dwipair) (idx' dwipair)

writeAcqparms :: FilePath -> [DWIPair] -> Action ()
writeAcqparms out dwipairs =
  writeFile' out (acq ++ acq')
  where
    acq = concatMap (unlines . acqparams) dwipairs
    acq' = concatMap (unlines . acqparams') dwipairs

writeB0s :: FilePath -> [DWIPair] -> Action ()
writeB0s out dwipairs = do
  writePosB0s "Pos_B0.nii.gz" dwipairs
  writeNegB0s "Neg_B0.nii.gz" dwipairs
  mergeVols out ["Pos_B0.nii.gz", "Neg_B0.nii.gz"]

writeIndex :: FilePath -> [DWIPair] -> Action ()
writeIndex out dwipairs = writeFile' out (unlines $ indexPos ++ indexNeg)
  where
    numPos = length $ concatMap b0s dwipairs
    numNeg = length $ concatMap b0s' dwipairs
    numPosB0s = length $ concatMap acqparams dwipairs
    indexPos = replicate numPos "0"
    indexNeg = replicate  numNeg (show numPosB0s)

writeCombined :: FilePath -> [DWIPair] -> Action ()
writeCombined out dwipairs
  = mergeVols out $ (map dwi dwipairs) ++ (map dwi' dwipairs)

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="build", shakeVerbosity=Chatty} $ do
    want ["build/topup/Pos_Neg_b0.nii.gz"]

    phony "clean" $ do
        putNormal "Cleaning files in build"
        removeFilesAfter "build" ["//*"]

    [ "build/topup/Pos_Neg.nii.gz",
      "build/topup/Pos_Neg_b0.nii.gz",
      "build/topup/acqparams.txt",
      "build/topup/index.txt"] *>> \[outvol, outb0s, acqparams, index] -> do
      need $ pas ++ aps
      let
        readDWIPair (dwi, dwi') =
          mkDWIPair <$>
            (pure dwi) <*>
            (pure dwi') <*>
            (readbval $ tobval dwi) <*>
            (readbval $ tobval dwi')
      dwiPairs <- traverse readDWIPair $ zip pas aps
      writeB0s outb0s dwiPairs
      writeCombined outvol dwiPairs
      writeIndex index dwiPairs
      writeAcqparms acqparams dwiPairs


    -- ["build/topup/Pos_Neg.nii.gz",
    --  "build/topup/Pos_Neg_b0.nii.gz",
    --  "build/topup/Pos_Neg.bval",
    --  "build/topup/Pos_Neg.bvec"] *>> \[outvol, outb0s, outbval, outbvec] -> do

    --   let invols =  ["build/topup/Pos.nii.gz", "build/topup/Neg.nii.gz"]
    --   let inb0s =  ["build/topup/Pos_b0.nii.gz", "build/topup/Neg_b0.nii.gz"]
    --   let inbvals = map tobval invols
    --   let inbvecs = map tobvec invols
    --   need $ invols ++ inb0s ++ inbvals ++ inbvecs
    --   mergeVols outvol invols
    --   mergeVols outb0s inb0s
    --   mergebvals outbval inbvals
    --   mergebvecs outbvec inbvecs
    --   trimVol outvol
    --   trimVol outb0s

    -- combineSeriesRule "Neg" aps
    -- combineSeriesRule "Pos" pas
    -- extractB0sRule "Pos" pas
    -- extractB0sRule "Neg" aps

-- topup --imain=${workingdir}/Pos_Neg_b0 --datain=${workingdir}/acqparams.txt --config=${topup_config_file} --out=${workingdir}/topup_Pos_Neg_b0 -v
