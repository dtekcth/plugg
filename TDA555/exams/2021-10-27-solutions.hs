import Data.List (permutations, sort)
import Test.QuickCheck

-- Question 1

thrice :: a -> [a]
thrice x = [x, x, x]

sums :: Num a => [a] -> [a]
sums (x:y:ys) = x : sums (x + y : ys)
sums xs       = xs

{-
  map thrice (sums [0..4])
=
  map thrice (sums [0,1,2,3,4])
=
  map thrice (0 : sums [1,2,3,4])
=
  map thrice (0 : 1 : sums [3,3,4])
= 
  map thrice (0 : 1 : 3 : sums [6,4])
= 
  map thrice (0 : 1 : 3 : 6 : sums [10])
= 
  map thrice (0 : 1 : 3 : 6 : [10])
=
  map thrice [0,1,3,6,10]
= 
  thrice 0 : map thrice [1,3,6,10]
= 
  [0,0,0] : map thrice [3,6,10]
= 
  ...
= 
  [[0,0,0],[1,1,1], [3,3,3], [6,6,6], [10,10,10]]
-}

-- Question 2

discriminant :: Double -> Double -> Double -> Double
discriminant a b c = b^2 - 4 * a * c

nRoots :: Double -> Double -> Double -> Int
nRoots a b c 
  | d <  0 = 0
  | d == 0 = 1
  | d >  0 = 2
 where
  d = discriminant a b c

data Root = None | One Double | Two Double Double

roots :: (Double, Double, Double) -> Root 
roots (a, b, c) = case nRoots a b c of
  0 -> None
  1 -> One (-b / 2*a)
  2 -> Two ((-b + sqrt d) / 2*a) ((-b - sqrt d) / 2*a)
 where
  d = discriminant a b c

-- Question 3

condMap1, condMap2, condMap3 :: (a -> Bool) -> (a -> a) -> [a] -> [a]
condMap1 p f [] = []
condMap1 p f (x:xs) 
  | p x       = f x : condMap1 p f xs
  | otherwise =   x : condMap1 p f xs

condMap2 p f xs = [if p x then f x else x | x <- xs]

condMap3 p f = map (\x -> if p x then f x else x)

replace :: Eq a => a -> a -> [a] -> [a]
replace x y = condMap1 (== x) (\_ -> y)

-- Question 4

instance Show Root where
  show r = case r of
    None    -> "no roots"
    One r   -> show r
    Two r s -> show r ++ " " ++ show s

instance Eq Root where
  None    == None    = True
  One a   == One b   = a == b
  Two a b == Two c d = (a, b) == (c, d) || (a, b) == (d, c)
  _       == _       = False

-- Question 5

readRoots :: String -> Root
readRoots txt = case words txt of
  ["-"]  -> None
  [x]    -> One (read x)
  [x, y] -> Two (read x) (read y)

askRoots :: (Double, Double, Double) -> IO ()
askRoots (a, b, c) = do
  putStr $ "What are the roots of: " ++ showEq a b c ++ "?" ++ 
           " Write '-' if there aren't any.\n> "
  answer <- getLine
  let rs = roots (a, b, c) in if readRoots answer == rs 
    then putStrLn "Well done!"
    else putStrLn $ "Bummer, the roots are: " ++ show rs
 where
  showEq a b c = showCoeff a ++ "x^2 + " ++ showCoeff b ++ "x + " ++ showCoeff c
  showCoeff c  | c == 1.0  = ""
               | otherwise = show c 

-- Question 6

prop_length :: [Int] -> Bool
prop_length xs = length xs == length (sort xs)

prop_min :: [Int] -> Bool
prop_min [] = True
prop_min xs = minimum xs == head (sort xs)

prop_ordered :: [Int] -> Bool
prop_ordered xs = ordered (sort xs)
 where
  ordered (x:y:ys) = x <= y && ordered (y:ys)
  ordered _        = True

-- Question 7

data Shop = Shop [Ski] deriving Show
data Ski = Ski 
  { brand   :: Brand 
  , skiType :: SkiType 
  , size    :: Size 
  , price   :: Price 
  } deriving Show

type Brand = String
type Price = Int

data SkiType 
  = Classic ClassicBinding 
  | Skate   SkateBinding
  | Skin    ClassicBinding 
  deriving Show

data Size = S182 | S187 | S192 | S197 | S202 | S207 | Custom Int 
  deriving Show

data ClassicBinding = Normal | MoveableRace | MoveableSwitch deriving Show
data SkateBinding   = SkateBinding deriving Show

getClassic :: Shop -> [Ski]
getClassic (Shop skis) = [ski | ski@(Ski _ (Classic _) _ _) <- skis]

-- Question 8

data SmallList = SL [Int] deriving (Eq, Show)

instance Arbitrary SmallList where
  arbitrary = do
    n  <- choose (0, 8)
    xs <- vectorOf n arbitrary
    return (SL xs)

propLength :: SmallList -> Bool
propLength (SL xs) = length (permutations xs) == fac (length xs)
 where
  fac n = product [1..n]

isPerm :: Eq a => [a] -> [a] -> Bool
isPerm xs ys = length xs == length ys && all (\x -> count x xs == count x ys) xs
 where
  count x xs = length (filter (== x) xs)

propPerm :: SmallList -> Bool
propPerm (SL xs) = all (isPerm xs) (permutations xs)

-- Question 9

data Expr 
  = Lit Literal         -- Literal, either integer or boolean
  | Add Expr Expr       -- Addition
  | Div Expr Expr       -- Integer division
  | Gth Expr Expr       -- Comparison operator
  | If  Expr Expr Expr  -- if-then-else 
  deriving (Show) 

data Literal 
  = N Int               -- Integer literal
  | B Bool              -- Boolean literal
  deriving (Eq, Show)

-- Smart constructor for integers
num :: Int -> Expr
num = Lit . N

-- Smart constructor for booleans
bool :: Bool -> Expr
bool = Lit . B

-- Example expressions
e1, e2, e3_bad, e4_bad :: Expr
e1 = (num 4 `Add` num 5) `Div` num 2
e2 = If (e1 `Gth` (num 4)) (num 3) (num 4 `Add` num 1)
e3_bad = If (num 4) (bool True) (num 0)
e4_bad = If (bool False) (num 42) (num 3 `Div` num 0)

eval :: Expr -> Maybe Literal
eval expr = case expr of
  Lit l -> Just l

  Add x y -> do
    x' <- eval x 
    y' <- eval y
    liftNum (+) x' y'
  Div x y -> do
    x' <- eval x
    y' <- eval y
    if y' /= N 0
      then liftNum div x' y'
      else Nothing
  
  Gth x y -> do
    x' <- eval x
    y' <- eval y
    case (x', y') of
      (N a, N b) -> Just $ B (a > b)
      _          -> Nothing
  
  If c x y -> do
    c' <- eval c
    case c' of
      B True  -> eval x
      B False -> eval y
      _       -> Nothing  -- Type error!
      
liftNum :: (Int -> Int -> Int) -> Literal -> Literal -> Maybe Literal
liftNum op (N x) (N y) = Just $ N (x `op` y)
liftNum _  _     _     = Nothing
