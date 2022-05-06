data PostfixValue = Op Char | Number Int deriving Show

data PostfixTree = Node (Maybe PostfixTree) PostfixValue (Maybe PostfixTree) deriving Show

-- quality of life functions
splitBy :: Char -> String -> [String]
splitBy c s = splitBy_ c s [] []

splitBy_ :: Char -> String -> String -> [String] -> [String]
splitBy_ _ [] [] akum = akum
splitBy_ _ [] (x:xs) akum = (x:xs):akum
splitBy_ c (x:xs) localAkum akum = if c == x then splitBy_ c xs [] (localAkum:akum)
    else splitBy_ c xs (x:localAkum) akum

isInt :: String -> Bool
isInt s = length (filter (\c -> not $ elem c "0123456789") s ) == 0

isOp :: Char -> Bool
isOp c = elem c "+-*/^"

-- postfixTree stuff
buildPostfixTree :: String -> Maybe PostfixTree
buildPostfixTree s = buildPostfixTree_ (reverse $ splitBy ' ' s) []

buildPostfixTree_ :: [String] -> [PostfixTree] -> Maybe PostfixTree
buildPostfixTree_ [] akum = if length akum == 1 then Just $ head akum
    else Nothing
buildPostfixTree_ (s:ss) (x:xs:xss) = if isInt s then buildPostfixTree_ ss (Node Nothing (Number (read s)) Nothing :(x:xs:xss))
    else 
        --let newTree = Just (Node (Just xs) (Op $ head s) (Just x))
        --let newAkum = newTree:xss
        if and [(isOp $ head s), (length s == 1)] then buildPostfixTree_ ss ((Node (Just x) (Op (head s)) (Just xs)):xss)
            else Nothing
buildPostfixTree_ (s:ss) [] = if isInt s then buildPostfixTree_ ss [Node Nothing (Number (read s)) Nothing]
    else Nothing
buildPostfixTree_ (s:ss) (x:xs) = if isInt s then buildPostfixTree_ ss ((Node Nothing (Number (read s)) Nothing):(x:xs))
    else Nothing

evalPostfix :: String -> Maybe Int
evalPostfix = evalPostfixTree . buildPostfixTree

evalPostfixTree :: Maybe PostfixTree -> Maybe Int
-- eval invalid tree
evalPostfixTree Nothing = Nothing
-- return number from leaf node
evalPostfixTree (Just (Node Nothing (Number val) Nothing)) = Just val
-- now the actual valid case
evalPostfixTree (Just (Node subtree1 (Op c) subtree2)) = case c of
    '+' -> evalPostfixTree_ (+) (evalPostfixTree subtree2) (evalPostfixTree subtree1)
    '-' -> evalPostfixTree_ (-) (evalPostfixTree subtree2) (evalPostfixTree subtree1)
    '*' -> evalPostfixTree_ (*) (evalPostfixTree subtree2) (evalPostfixTree subtree1)
    '/' -> do
        let numerator = evalPostfixTree subtree2
        let denominator = evalPostfixTree subtree1
        if denominator /= Just 0 then evalPostfixTree_ div numerator denominator
            else Nothing
    '^' -> evalPostfixTree_ (^) (evalPostfixTree subtree2) (evalPostfixTree subtree1)
    _ -> Nothing
-- invalid
-- evalPostfixTree (Just (Node _ (Number _) (Just _))) = Nothing
-- evalPostfixTree (Just (Node (Just _) (Number _) _)) = Nothing
evalPostfixTree (Just (Node _ _ _)) = Nothing

evalPostfixTree_ :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> Maybe Int
evalPostfixTree_ f (Just val1) (Just val2) = Just (f val1 val2)
evalPostfixTree_ _ _ _ = Nothing