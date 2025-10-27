-- a) implementieren win mit Pattern Matching
win :: Int -> Int -> Int
win _ 0 = 0
win a b
  | a < b = win b a
  | a `mod` 10 == 0 = b + win (a - 1) (b - 1)
  | otherwise = b + win (a - 1) b


-- b) optimieren win-Funktion mit weniger Speicher
-- Lazy Evaluation: Haskell postpones calculations until it nees -> it uses lots of memory
-- Use tail recursion + strict evaluation
winTR :: Int -> Int -> Int -> Int
winTR _ 0 acc = acc
winTR a b acc
  | a < b = winTR b a acc
  | a `mod` 10 == 0 = winTR (a - 1) (b - 1) $! (b + acc)
  | otherwise = winTR (a - 1) b (b + acc)

win2 :: Int -> Int -> Int
win2 a b = winTR a b 0

-- c) implementieren fib-Funktion und optimieren die Funktion