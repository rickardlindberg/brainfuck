data BinaryTree = Node BinaryTree BinaryTree | Leaf Int


printTree (Node left right) =
    printTreeI left " " >> printTreeI right " "
printTree (Leaf leaf) = print leaf

printTreeI (Node left right) indent =
    printTreeI left (indent ++ " ") >> printTreeI right (indent ++ " ")
printTreeI (Leaf leaf) indent = putStr indent >> print leaf

main = do
    printTree $ Node (Node (Leaf 1) (Leaf 2))(Node (Leaf 3) (Leaf 4))
    printTree $ Leaf 11
    printTree $ Node (Node (Node (Leaf 1) (Leaf 5)) (Leaf 2))(Node (Leaf 3) (Leaf 4))
