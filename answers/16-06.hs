
data Tree = Leaf Int | Node Tree Tree

leaves :: Tree -> Int
leaves (Leaf _) = 1
leaves (Node l r) = leaves l + leaves r

nodes :: Tree -> Int
nodes (Leaf _) = 0
nodes (Node l r) = 1 + nodes l + nodes r

tree :: Tree
tree = Node (Node (Leaf 1)  (Leaf 2)) (Leaf 3)

{-

基底部：
     leaevs (Leaf n) - leaves (Leaf n)
        {leavesとnodesを適用}
   = 1 - 0
   = 1

再帰部：
     leaves (Node l r) - nodes (Node l r)
        {leavesとnodesを適用}
   = (leaves l + leaves r) - (1 + nodes l + nodes r)
        {括弧を展開}
   = leaves l + leaves r - 1 - nodes l - nodes r
        {順序を入れ替える}
   = leaevs l - nodes l + leaves r - nodes r - 1
        {仮定より}
   = 1 + 1 -1
        {+, -を適用}
   = 1

-}
