
data Elements a = Element a | SubList [Elements a] deriving (Show, Eq)

flatten :: [Elements a] -> [Elements a]

flatten [Element a] = [Element a]
flatten [SubList a] = (flatten a)
flatten [] = []
flatten ls = (flatten [(head ls)]) ++ (flatten (tail ls))


