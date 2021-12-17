module Day1 (
 part1,
 part2
)
where

part1 :: IO ()
part1 = do
  file <- readFile "/Users/abhishekverma/IdeaProjects/AdventOfCode/out/production/AdventOfCode/Day1_input.txt"
  let depths = map read (lines file) :: [Int]
  print depths
  let increases = sum [ if i < j then 1 else 0 | (i, j) <- zip depths (tail depths) ]
  putStrLn ("Part 1: " ++ show increases)

part2 :: IO ()
part2 = do
  file <- readFile "/Users/abhishekverma/IdeaProjects/AdventOfCode/out/production/AdventOfCode/Day1_input.txt"
  let depths = map read (lines file) :: [Int]
  print depths
  let sum3 = [ i + j + k | (i, j, k) <- zip3 depths (tail depths) (tail (tail depths))]
  let increases = sum [ if i < j then 1 else 0 | (i, j) <- zip sum3 (tail sum3) ]
  putStrLn ("Part 2: " ++ show increases)