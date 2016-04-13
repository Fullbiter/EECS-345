myfunction :: a -> Maybe [a] -> (a -> Bool) -> Maybe [a]

myfunction x vls f = do
    ls <- vls
    if (f x) then return (x : ls)
        else Nothing

checklist [] f = Just []
checklist ls f = (myfunction (head ls) (checklist (tail ls) f) f)

checkappend vls1 vls2 f = do
    ls1 <- vls1
    ls2 <- vls2
    if (checklist ls1 f) == Nothing then Nothing
        else return (ls1 ++ ls2)
