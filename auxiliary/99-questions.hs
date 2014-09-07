-- Problem 8
compress' :: (Eq a) => [a] -> [a]
compress' xs = let squish x [] = [x]
                   squish x acc
                     | x == head acc = acc
                     | otherwise = x:acc
               in foldr squish [] xs

-- Problem 15
replicate' :: [a] -> Int -> [a]
replicate' xs n = foldl (\acc x -> acc ++ (replicate n x)) [] xs
