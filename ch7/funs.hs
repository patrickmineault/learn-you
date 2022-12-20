import qualified Data.Map as Map

member' :: (Eq k, Ord k) => k -> Map.Map k v -> Bool
member' key theMap = case (Map.lookup key theMap) of
     Nothing -> False
     Just _ -> True