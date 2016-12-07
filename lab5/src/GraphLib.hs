{-# LANGUAGE TupleSections #-}

module GraphLib (fromEdgeList, fromConnectivityMatrix) where

fromEdgeList :: Int -> [(Int, Int)] -> (Int, [(Int, Int)])
fromEdgeList = (,)

fromConnectivityMatrix :: [[Int]] -> (Int, [(Int, Int)])
fromConnectivityMatrix m = (length m, ff m) where
    ff = concatMap (\(i,js) -> (i,) <$> f js) . zip [1..]
    f  = map fst . filter ((==1) . snd) . zip [1..]
