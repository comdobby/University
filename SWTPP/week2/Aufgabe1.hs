-- a) implementieren win mit Pattern Matching
win :: Int -> Int -> Int
win _ 0 = 0
win a b
  | a < b = win b a
  | a `mod` 10 == 0 = b + win (a - 1) (b - 1)
  | otherwise = b + win (a - 1) b

-- b)