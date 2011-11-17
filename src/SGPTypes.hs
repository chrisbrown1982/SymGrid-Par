module SGPTypes where

data Arith = List [Arith]
           | Rational [Arith]
           | Matrix [Arith]
           | MatrixRow [Arith]
           | Mul FiniteField Int
           | Power FiniteField Int
           | Polynomial String
           | Num Integer
           | Empty
                deriving (Show, Read, Eq, Ord)

--data MatrixRow = MatrixRow [Arith]
--                    deriving (Show, Read, Eq, Ord)

data FiniteField = PrimEl Int
                     deriving (Show, Read, Eq, Ord)

{-data Arith = Matrix [Arith] | MatrixRow [Arith] | Mul FiniteField Int | Power FiniteField Int | Num Int 
                deriving (Show, Read, Eq, Ord)

--data MatrixRow = MatrixRow [Arith]
--                    deriving (Show, Read, Eq, Ord)

data FiniteField = PrimEl Int
                     deriving (Show, Read, Eq, Ord) -}
