mergeSort :: (Ord a) => [a] -> [a]
mergeSort xs = concat $ mergeSort' [[x] | x <- xs]

mergeSort' :: (Ord a) => [[a]] -> [[a]]
mergeSort' [] = []
mergeSort' [xs] = [xs]
mergeSort' (xs:ys:zss) = mergeSort' $ (mergeSortedLists [] xs ys):zss

mergeSortedLists :: (Ord a) => [a] -> [a] -> [a] -> [a]
mergeSortedLists sorted [] [] = reverse sorted
mergeSortedLists sorted xs [] = reverse sorted ++ xs
mergeSortedLists sorted [] ys = reverse sorted ++ ys
mergeSortedLists sorted (x:xs) (y:ys)
  | x < y = mergeSortedLists (x:sorted) xs (y:ys)
  | otherwise = mergeSortedLists (y:sorted) (x:xs) ys
