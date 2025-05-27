mygetLine :: IO String
mygetLine = getLine' ""

getLine' :: String -> IO String
getLine' xs =
   do x <- getChar
      case x of
         '\n'      -> return xs
         '\DEL'    -> do putChar '\b'
                         putChar '\b'
                         putChar '\b'
                         putChar ' '  -- delete 'D'
                         putChar ' '  -- delete 'E'
                         putChar ' '  -- delete 'L'
                         putChar '\b'
                         putChar '\b'
                         putChar '\b'
                         ys <- getLine' (init xs)
                         return ys
         otherwise -> do ys <- getLine' (xs ++ [x])
                         return ys

