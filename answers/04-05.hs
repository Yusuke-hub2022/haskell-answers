(&&) :: Bool -> Bool -> Bool
a && b = if a == b then
    if a then a
    else b
else False

-- $ ghci 04-05.hS
-- ghci> True Main.&& True
-- True
