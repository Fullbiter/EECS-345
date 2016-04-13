
data Elements a = Element a | SubList [Elements a] deriving (Show, Eq)

flatten :: [Elements a] -> [Elements a]

flatten [SubList a] = (flatten a)
flatten [Element a] = [Element a]
flatten [] = []
flatten ls = (flatten [(head ls)]) ++ (flatten (tail ls))

myreverse :: [Elements a] -> [Elements a]

myreverse [SubList a] = [SubList (myreverse a)]
myreverse [Element a] = [Element a]
myreverse [] = []
myreverse ls = (myreverse (tail ls)) ++ (myreverse [(head ls)])
