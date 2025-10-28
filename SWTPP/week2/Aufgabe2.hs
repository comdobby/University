-- Verwendet deriving Show
-- deriving Show konveriert Datentyp in String

-- a) implementieren Aufzählungstyp CommandS, CommandS hat Put, Take und Win

-- Hilfsfunktionen
fee :: Int
fee = 1

charge :: Int -> Int
charge val = if val == 0 then 0 else val - fee

putChips :: Int -> Int -> Int
putChips owned added = charge (owned + added)

takeChips :: Int -> Int -> Int
takeChips owned taken = if owned < taken then 0 else owned - taken

winTR :: Int -> Int -> Int -> Int
winTR _ 0 acc = acc
winTR a b acc
  | a < b = winTR b a acc
  | a `mod` 10 == 0 = winTR (a - 1) (b - 1) $! (b + acc)
  | otherwise = winTR (a - 1) b $! (b + acc)

win :: Int -> Int -> Int
win a b = winTR a b 0

-- CommandS
data CommandS = Put | Take | Win deriving Show

-- eval
eval :: CommandS -> Int -> Int -> Int
eval Put = putChips
eval Take = takeChips
eval Win = win

-- b) erweitern CommandS -> CommandP(Summen- und Produkttyp)
data CommandP = PutP Int Int | TakeP Int Int | WinP Int Int deriving Show

evalP :: CommandP -> Int
evalP (PutP owned added) = putChips owned added
evalP (TakeP owned taken) = takeChips owned taken
evalP (WinP a b) = win a b