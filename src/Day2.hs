module Day2 (
  part1,
  part2
) where

execute1 :: String -> (Int, Int)
execute1 command
 | motion == "forward" = (distance, 0)
 | motion == "down" = (0, distance)
 | motion == "up" = (0, -distance)
 | otherwise = (-1, -1)
 where
   motion = head (words command)
   distance = read (last (words command)) :: Int

process1 :: [String] -> Int
process1 commands = do
    let xy = map execute1 commands
    sum (map fst xy) * sum (map snd xy)

part1 :: IO ()
part1 = do
  file <- readFile "/Users/abhishekverma/IdeaProjects/AdventOfCode/src/Day2_input.txt"
  putStrLn file
  print (process1 (lines file))

execute2 :: String -> (Int, Int, Int) -> (Int, Int, Int)
execute2 command (ox, oy, oa)
 | motion == "forward" = (ox+distance, oy + oa*distance, oa)
 | motion == "down" = (ox, oy, oa+distance)
 | motion == "up" = (ox, oy, oa-distance)
 | otherwise = (-1, -1, -1)
 where
   motion = head (words command)
   distance = read (last (words command)) :: Int
   
process2 :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
process2 [] (x, y, z) = (x, y, z)
process2 (l:ls) (x, y, z) = process2 ls (execute2 l (x, y, z)) 
   
part2 :: IO ()
part2 = do
  file <- readFile "/Users/abhishekverma/IdeaProjects/AdventOfCode/src/Day2_input.txt"
  putStrLn file
  let (x, y, _) = process2 (lines file) (0,0,0)
  print (show (x*y))
  