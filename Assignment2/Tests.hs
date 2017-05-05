import SA
import Utilities

main = do
  putStrLn "\nString 1: writers, String 2: vintner\n"

  putStrLn $ "SimilarityScore = " ++ show (similarityScore "writers" "vintner")
  putStrLn $ "SimilarityScore2 = " ++ show (similarityScore2 "writers" "vintner") ++ "\n"

  putStrLn $ "optAlignments = "
  mapM_ (putStrLn . showTup) (optAlignments "writers" "vintner")
  putStrLn $ "optAlignments2 = "
  mapM_ (putStrLn . showTup) (optAlignments2 "writers" "vintner")

  outputOptAlignments "writers" "vintner"

  putStrLn "\nString 1: aferociousmonadatemyhamster, String 2: functionalprogrammingrules\n"

  putStrLn $ "SimilarityScore2 = " ++ show (similarityScore2 "aferociousmonadatemyhamster" "functionalprogrammingrules") ++ "\n"

  -- putStrLn $ "optAlignments2 = "
  -- mapM_ (putStrLn . showTup) (optAlignments2 "aferociousmonadatemyhamster" "functionalprogrammingrules")

  -- outputOptAlignments "aferociousmonadatemyhamster" "functionalprogrammingrules"
