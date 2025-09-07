{-

基底部：
   
   comp' (Val n) c
      {仮定より}
   = comp (Val n) ++ c
      {comを適用}
   = [PUSH n] ++ c
      {リストの性質より}
   = PUSH n : c

再帰部:

   comp' (Add x y) c
      {仮定より}
   = comp (Add x y) ++ c
      {compを適用}
   = (comp x ++ comp y ++ [ADD]) ++ c
      {++の結合則より}
   = comp x ++ comp y ++ [ADD] ++ c
      {++の結合則より}
   = comp x ++ (comp y ++ [ADD] ++ c)
      {xに対する仮定より}
   = comp' x (comp y ++ [ADD] ++ c)
      {++の結合則より}
   = comp' x (comp y ++ ([ADD] ++ c))
      {yに対する仮定より}
   = comp' x (comp' y ([ADD] ++ c))
      {リストの性質より}
   = comp' x (comp' y (ADD : c))

-}
