--Parse expressions like these:
--10 4 3 + 2 * -
rpnEval :: (Num b, Read b) => [String] -> b
rpnEval a = head $ foldl foldingFun [] a
    where 
        foldingFun (a:b:xs) "*" = (a*b):xs
        foldingFun (a:b:xs) "+" = (a+b):xs
        foldingFun (a:b:xs) "-" = (b-a):xs
        foldingFun xs theVar = read theVar:xs


main :: IO ()
main = do
    let str = "10 4 3 + 2 * -"
    let stack = words str
    print . show $ rpnEval stack
