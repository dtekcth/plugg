{- This file contains code for solving each of the three exam problems 
   Explanatory texts are mostly not included other than a few comments. 
-}
import Test.QuickCheck


-- *** P1 ***
{- You could use a record type here, and even use it to automatically create the radius and position functions 
Having a pair for positions is not necessary but it makes it less likely to mix up radius and coordinates. 
-}
data Circle = Circle Double (Double, Double) deriving (Show, Eq)

radius :: Circle -> Double
radius (Circle r _) = r

position :: Circle -> (Double, Double)
position (Circle _ p) = p

move :: Circle -> (Double, Double) -> Circle
move (Circle r (x,y)) (deltax, deltay) = Circle r (x+deltax,y+deltay)

scale :: Circle -> Double -> Circle 
scale (Circle r p) s = Circle (r*s) p

distance :: Circle -> Circle -> Double
distance (Circle _ (x1,y1)) (Circle _ (x2,y2)) = 
    sqrt ((x1-x2)^2 + (y1-y2)^2)

{- Two circles touch/overlap if their combined radius equals or exceeds the distance between their centers -}
collides :: Circle -> Circle -> Bool
collides c1 c2 = 
    distance c1 c2 <= radius c1 + radius c2

c1 = Circle 2 (-2,2)
c2 = Circle 1.5 (1, 3)
c3 = Circle 1 (1,-2)

-- Required for excellent
instance Arbitrary Circle where 
  arbitrary = do 
    -- One of several ways to generate arbitrary non-negative numbers
    -- Just applying abs to an arbitrary value is also OK
    (NonNegative r) <- arbitrary
    pos <- arbitrary
    return (Circle r pos)


-- *** P2 ***

{- This solution uses explicit recursion in a helper function called go. 
   You could solve it more elegantly using some prelude functions and/or a list comprehension. 
   
   go is always called with the two upcoming values in the sequence (in order) and it produces the whole sequence recursively. 
   
-}
intsequence :: Int -> Int -> Int -> Int -> Int -> [Int]
intsequence x0 x1 m1 m2 add = go x0 x1 
  where 
    go current next = current : go next (next*m1 + current*m2 + add)

{- An alternative compact "one liner" solution, mostly for fun.
   Also showcases an advanced use of recursion and lazy evaluation: 
   We build r by zipping r together with the tail of r! 
-}
intsequence' x0 x1 m1 m2 a = 
  let r = x0:x1:[xn1*m1+xn2*m2+a|(xn1,xn2) <- zip (tail r) r] in r

-- Required for Excellent
-- So many Int in this type signature :)
prop_seq :: Int -> Int -> Int -> Int -> Int -> Int -> Bool
prop_seq n x0 x1 m1 m2 a = case drop n (intsequence x0 x1 m1 m2 a) of
  (v2:v1:v:_) -> v == v1*m1 + v2*m2 + a
  _           -> False -- The list is finite! Something is very wrong.

-- ** Test code, not part of solution **
t2_1 = print (take 10 $ intsequence 0 1 1 1 0)
t2_2 = print (take 10 $ intsequence 3 6 1 0 3)
t2_3 = print (take 10 $ intsequence 1 2 2 0 0)



-- *** P3 ***
{- Helper function to check is a position is valid -}
isValid :: Position -> Bool
isValid (x,y) = x >= 1 && x <= 8 && y >= 1 && y <= 8

{- Helper function to just get the owner of a position, or nothing if it is free-}
getOwner :: Game -> Position -> Maybe Player
getOwner g pos = fmap snd (getPiece g pos)

-- 
bishopMoves :: Game -> Position -> Player -> [Position]
bishopMoves g origin p = 
  bishopDir origin (-1,-1) ++
  bishopDir origin (-1,1) ++
  bishopDir origin (1,-1) ++
  bishopDir origin (1,1)
  where
    {- Computes all valid moves from a given position (x,y) not including (x,y),
       moving in a given direction (dx, dy) specified as position change per step
    -}
    bishopDir (x,y) (dx, dy) 
      -- The next position is outside the board or occupied by a friendly piece
      -- We can not move further in this direction
      | not (isValid pos) || owner == Just p = []
      -- The next cell is free, add it as a possible move and keep going
      | owner == Nothing                     = pos : bishopDir pos (dx,dy)
      -- The next position is occupied by an opponents piece
      -- We can move to capture it but not move further beyond it
      | otherwise                            = [pos]
      where -- pos is the next position in the given direction
            pos = (x+dx,y+dy)
            -- The current owner of pos
            owner = getOwner g pos


-- Required for Excellent
-- This one just answers true/false, not which moves it could to make
bishopCheck :: Game -> Position -> Player -> Bool
bishopCheck g origin p = any isTarget twomoves
  where 
    movesFrom pos = bishopMoves g pos p
    -- All positions reachable in two moves (with lots of duplicates)
    twomoves = concatMap movesFrom (movesFrom origin)
    -- Does pos contain a king? (It's the opponents king if we can move to it)
    isTarget pos = fmap fst (getPiece g pos) == Just King

    

-- Test code, not part of solution

-- A position on a chessboard 
--  For valid postions both numbers should be between 1 and 8 inclusive
type Position = (Int, Int)
-- Represents an ongoing chess round, including all current positions of pieces
type Game = [(Position, (Piece, Player))]
-- All types of chesspieces
data Piece = King | Bishop | Pawn deriving (Show, Eq)
-- The two players of a chess game, White and Black.
data Player = White | Black deriving (Show, Eq)

-- getPiece g p will give the chesspiece occupying position p on the gameboard g, 
--   along with the player that owns it, or Nothing if the position is not occupied
--   Will crash for invalid positions.
getPiece :: Game -> Position -> Maybe (Piece, Player)
getPiece g pos = lookup pos g

-- This is the board shown in the exam text
testgame = [((4,2), (Pawn, Black))
           ,((4,6), (Pawn, White))
           ,((6,4), (Bishop, Black))]

t3_1 = print (bishopMoves testgame (6,4) Black)

