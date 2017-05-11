module SA where
import Utilities
import Data.List

type AlignmentType = (String, String)

--------------------------------------------------------------------------

similarityScore :: String -> String -> Int
similarityScore [] [] = 0
similarityScore [] (s2:ss2) = score '-' s2 + similarityScore [] ss2
similarityScore (s1:ss1) [] = score s1 '-' + similarityScore ss1 []
similarityScore (s1:ss1) (s2:ss2) = maximum 
                                    [score s1 s2 + similarityScore ss1 ss2,
                                    score s1 '-' + similarityScore ss1 (s2:ss2),
                                    score '-' s2 + similarityScore (s1:ss1) ss2]

optAlignments :: String -> String -> [AlignmentType]
optAlignments [] [] = [([], [])]
optAlignments [] (s2:ss2) = attachHeads '-' s2 $ optAlignments [] ss2 
optAlignments (s1:ss1) [] = attachHeads s1 '-' $ optAlignments ss1 []
optAlignments (s1:ss1) (s2:ss2) = maximaBy stringScore $ concat
                                  [attachHeads s1 s2 (optAlignments ss1 ss2),
                                  attachHeads '-' s2 (optAlignments (s1:ss1) ss2),
                                  attachHeads s1 '-' (optAlignments ss1 (s2:ss2))]

--------------------------------------------------------------------------

similarityScore2 :: String -> String -> Int
similarityScore2 xs ys = mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> Int
    mcsEntry 0 0 = 0
    mcsEntry i 0 = scoreSpace + mcsLen (i-1) 0
    mcsEntry 0 j = scoreSpace + mcsLen 0 (j-1)
    mcsEntry i j = score x y + maximum 
                   [mcsLen (i-1) (j-1), 
                   mcsLen (i-1) j,
                   mcsLen i (j-1)]
      where
         x = xs!!(i-1)
         y = ys!!(j-1)

optAlignments2 :: String -> String -> [AlignmentType]
optAlignments2 xs ys = snd $ mcsLen (length xs) (length ys)
  where
    mcsLen i j = mcsTable!!i!!j
    mcsTable = [[ mcsEntry i j | j<-[0..]] | i<-[0..] ]
       
    mcsEntry :: Int -> Int -> (Int, [AlignmentType])
    mcsEntry 0 0 = (0, [([],[])])
    mcsEntry i 0 = (scoreSpace + fst (mcsLen (i-1) 0), attachTails (xs!!(i-1)) '-' $ snd $ mcsLen (i-1) 0)
    mcsEntry 0 j = (scoreSpace + fst (mcsLen 0 (j-1)), attachTails '-' (ys!!(j-1)) $ snd $ mcsLen 0 (j-1))
    mcsEntry i j = ((fst . head) tmp, concatMap snd tmp)
      where
         x = xs!!(i-1)
         y = ys!!(j-1)
         tmp = maximaBy fst
               [(score x y + fst (mcsLen (i-1) (j-1)), attachTails x y $ snd $ mcsLen (i-1) (j-1)),
               (score '-' y + fst (mcsLen i (j-1)), attachTails '-' y $ snd $ mcsLen i (j-1)),
               (score x '-' + fst (mcsLen (i-1) j), attachTails x '-' $ snd $ mcsLen (i-1) j)]

--------------------------------------------------------------------------

outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
  putStrLn $ "\nThere are " ++ show (length result) ++ " optimal alignments:\n"
  mapM_ (putStrLn . showTup) result
  putStrLn $ "There were " ++ show (length result) ++ " optimal alignments!\n"
  where result = optAlignments2 string1 string2
  