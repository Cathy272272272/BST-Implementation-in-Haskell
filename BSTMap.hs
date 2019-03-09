module BSTMap (
    BSTMap,
    empty,
    insert,
    BSTMap.lookup
)where

data BSTMap k v = Nil
    | Node (k,v) (BSTMap k v) (BSTMap k v)
--denote an empty tree
empty :: BSTMap k v
empty = Nil
-- if equal, then find the object
-- if less than, search left subtree
-- if greater than, search right subtree
lookup :: (Ord k) => k -> BSTMap k v -> Maybe v
lookup _ Nil = Nothing
lookup x (Node (k,v) left right)
    | x == k = Just v
    | x > k = BSTMap.lookup x right
    | otherwise = BSTMap.lookup x left
-- if equal, then update the ovalue
-- if less than, go to left subtree
-- if greater than, go to right subtree
insert :: (Ord k) => k -> v -> BSTMap k v -> BSTMap k v
insert x y Nil = (Node (x, y) Nil Nil)   
insert x y (Node (k, v) left right)
    | x > k = (Node (k, v) left (insert x y right))
    | x < k = (Node (k, v) (insert x y left) right)
    | x == k = (Node(k, y) left right)

-- a helper function that compares every nodes recursively
equal :: (Eq k, Eq v) => BSTMap k v -> BSTMap k v -> Bool
equal Nil Nil = True
equal _ Nil = False
equal Nil _ = False
equal (Node(k0, v0) left0 right0) (Node(k1, v1) left1 right1)
    | (k0 /= k1 || v0 /= v1) = False
    | otherwise = (equal left0 left1 && equal right0 right1)
-- count all nodes recursively
kvPairs :: (Ord k, Ord v) => BSTMap k v -> Int
kvPairs Nil = 0
kvPairs (Node(k, v) left right)  = 1 + kvPairs left + kvPairs right
-- a helper function for showing BSTMap, using inorder traversal, since inorder traversal will show smaller keys first
inorder :: (Ord k, Ord v, Show k, Show v) => BSTMap k v -> String
inorder Nil = ""
inorder (Node (k, v) left right) = inorder left ++ (show k ++ ":" ++ show v ++ "\n" ) ++ inorder right 

--using equal to check whether the two BSTMap are equal
instance (Eq k, Eq v)  => Eq(BSTMap k v) where
    (==) b1 b2 = equal b1 b2
--using kvPairs by counting all key value pairs in the trees to compare
instance (Ord k, Ord v) => Ord(BSTMap k v) where
    compare b1 b2 = compare (kvPairs b1) (kvPairs b2) 
--if the tree is empty, then show "Nil"
--otherwise, do inorder traversal
instance (Show k, Show v, Ord k, Ord v) => Show(BSTMap k v) where
    show Nil = "Nil"
    show b = inorder b