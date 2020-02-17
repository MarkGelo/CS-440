import Data.List

--Problem 1
common :: Eq a => [a] -> [a] -> ([a],[a],[a])
common x y = common' x y
    where
        common' x1 [] = ([],x1,[])
        common' [] y1 = ([],[],y1)
        common' x1 y1 = (cp,x',y')
            where
                cp = commonPrefix x1 y1
                x' = x1 \\ cp
                y' = y1 \\ cp
                
-- assistant tail recursion
commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix [] _ = []
commonPrefix (x:xs) (y:ys)
  |x == y = y : commonPrefix xs ys
  |otherwise = []

--Problem 2
data List a = Node a (List a) | Nil deriving (Eq, Show,Read)
listShow test =
  case test of
    Nil -> "Nil"
    Node a Nil -> "Node " ++ show a ++ " Nil"
    Node a b -> "Node " ++ show a ++ " (" ++ listShow b ++ ")"

--Test Cases
x = Node 1 (Node 2 (Node 3 Nil))
y = Node 1 Nil
z = Node 1 (Node 2 (Node 3 (Node 4 Nil)))

--Problem 3
--added a Nil so that if theres a Nil then not a full tree
data Tree a b = Leaf b | Node1 a (Tree a b) (Tree a b)|Nil1 deriving (Read, Show, Eq)
isFull :: Tree a b -> Bool
isFull test =
  case test of
    Nil1 -> False
    Leaf l -> True
    Node1 a b c -> isFull b && isFull c

--Test Cases
x1 = Node1 1 (Leaf 1) (Leaf 1)
x2 = Leaf 1
x3 = Node1 1 (Node1 1 (Leaf 2) (Leaf 3)) (Leaf 1)
x4 = Node1 1 (Nil1) (Node1 1 (Leaf 2) (Leaf 3)) -- should be false
x5 = Node1 1 (Node1 2 (Leaf 2) (Leaf 3)) (Node1 3 (Leaf 4) (Leaf 5))
x6 = Node1 1 (Leaf 5) (Nil1) -- shoulde be false

--Problem 4
eval tree =
  case tree of
    Leaf num -> num
    Node1 a b c -> op (eval b) (eval c)
      where
        op =
          case a of
            "+" -> (+)
            "-" -> (-)
            "*" -> (*)
            "/" -> (/)

--works for +, -, * but stops working when I added /

--Test Cases
y1 = Node1 "+" (Leaf 2) (Leaf 4)
y2 = Node1 "-" (Leaf 11) (Leaf 8)
y3 = Node1 "/" (Node1 "*" y1 y2) (Leaf 36)
y4 = Node1 "*" (Node1 "*" (Leaf 2) (Leaf 3)) (Leaf 4)
{-

Problem 5
^0|([1-9]([\,]\d\d\d)+|[1-9]\d([\,]\d\d\d)+|[1-9]\d\d([\,]\d\d\d)+)|[1-9]\d\d|[1-9]\d|[1-9]$

Problem 6
^(\S+\s)*\S+$

Problem 7
from problem description: im asuming it only wants at most 4 lower case letters without w
so also no special symbols/characters, and no upper case and no numbers
^[^w\WA-Z0-9]{0,4}$

-}



