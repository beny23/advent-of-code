navigate :: (Int, Int, Int) -> [String] -> (Int, Int, Int)
navigate (f, d, a) ["forward", n] =
  let x = read n
  in (f + x, d + a * x, a)
navigate (f, d, a) ["down", n] = (f, d, a + read n)
navigate (f, d, a) ["up", n] = (f, d, a - read n)

main :: IO ()
main = do
  input <- getContents
  let directions = map words $ lines input
  print $ (\(f, d, a) -> f * d) $ foldl navigate (0, 0, 0) directions
