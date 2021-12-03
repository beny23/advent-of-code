navigate :: (Int, Int) -> [String] -> (Int, Int)
navigate (f, d) ["forward", n] = (f + read n, d)
navigate (f, d) ["down", n] = (f, d + read n)
navigate (f, d) ["up", n] = (f, d - read n)

main :: IO ()
main = do
  input <- getContents
  let directions = map words $ lines input
  print $ (\(f, d) -> f * d) $ foldl navigate (0, 0) directions
