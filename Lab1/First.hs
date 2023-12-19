{-# OPTIONS_GHC #-}
module First where
    import Data.Char (toUpper, toLower, isLower, isUpper)
    main :: IO ()

    -- Простейшие функции

    sumMy :: Num a => a -> a -> a
    sumMy a b = a + b
    
    productMy :: Num a => a -> a -> a
    productMy a b = a * b
    
    maxMy :: Ord a => a -> a -> a
    maxMy a b = if a > b then a else b
    
    minMy :: Ord a => a -> a -> a
    minMy a b = if a < b then a else b
    
    maximumMy :: Ord a => [a] -> a
    maximumMy [x] = x 
    maximumMy (x:xs) = maxMy x (maximumMy xs)

    minimumMy :: Ord a => [a] -> a
    minimumMy [x] = x
    minimumMy (x:xs) = minMy x (minimumMy xs)
    
    evenMy :: Integral a => a -> Bool
    evenMy x = x `mod` 2 == 0
    
    oddMy :: Integral a => a -> Bool
    oddMy x = x `mod` 2 /= 0

    gcdMy :: Integral t => t -> t -> t -- Наибольший общий делитель
    gcdMy a b 
        | b == 0 = a 
        | otherwise = gcdMy b (mod a b)

    lcmMy :: Integral a => a -> a -> a -- Наименьшее общее кратное
    lcmMy a b = abs (a * b) `div` gcdMy a b

    powerMy :: (Eq t1, Num t1, Num t2) => t2 -> t1 -> t2
    powerMy base exp
        | exp == 0 = 1
        | otherwise = base * powerMy base (exp - 1)

    factMy :: Integer -> Integer
    factMy 0 = 1
    factMy n = n * factMy (n - 1)

    fibMy :: Integer -> Integer
    fibMy 0 = 0
    fibMy 1 = 1
    fibMy n = fibMy (n - 1) + fibMy (n - 2)

    -- Улучшенная функция фибоначчи
    fibMy' :: Integer -> Integer -> Integer -> Integer
    fibMy' 0 _ prevprev = prevprev
    fibMy' 1 prev _ = prev
    fibMy' n prev prevprev = fibMy' (n - 1) (prev + prevprev) prev

    -- Логические функции

    andMy :: [Bool] -> Bool
    andMy [] = True
    andMy (x:xs) = x && andMy xs

    orMy :: [Bool] -> Bool
    orMy [] = False
    orMy (x:xs) = x || orMy xs

    -- Списочные функции

    headMy :: [a] -> a
    headMy (x:xs) = x

    tailMy :: [a] -> [a]
    tailMy (x:xs) = xs
    
    lastMy :: [a] -> a
    lastMy xs = headMy (reverse xs)
    
    initMy :: [a] -> [a]
    initMy xs = reverse (tailMy (reverse xs))
    
    lengthMy :: Num a1 => [a2] -> a1
    lengthMy xs = sum (map (\_ -> 1) xs)
    
    indexMy :: [a] -> Int -> a
    indexMy xs n = xs !! n

    concatMy :: [a] -> [a] -> [a]
    concatMy xs ys = xs ++ ys
    
    -- Берёт первые n элементов
    takeMy :: (Ord t, Num t) => t -> [a] -> [a]
    takeMy n xs 
        | n <= 0 = []
        | otherwise = headMy xs : takeMy (n - 1) (tailMy xs)
    
    -- Удаляет первые n элементов
    dropMy :: (Ord t, Num t) => t -> [a] -> [a]
    dropMy n xs
        | n <= 0 = xs
        | otherwise = dropMy (n - 1) (tailMy xs)
    
    reverseMy :: [a] -> [a]
    reverseMy [] = []
    reverseMy (x:xs) = reverseMy xs ++ [x]
    
    elemMy :: Eq a => a -> [a] -> Bool
    elemMy x [] = False
    elemMy x (y:ys) = (x == y) || elemMy x ys
    
    -- Повторить n раз значение x
    replicateMy :: Int -> a -> [a]
    replicateMy 0 _ = []
    replicateMy n x
        | n == 0 = []
        | otherwise = x : replicateMy (n-1) x 

    lookupMy :: Eq a => a -> b -> [(a, b)] -> b
    lookupMy xa xb [] = xb
    lookupMy xa xb ((k, v):kvs)
        | xa == k = v
        | otherwise = lookupMy xa xb kvs

    substrMy :: [a] -> Int -> Int -> [a]
    substrMy xs start end = takeMy (end - start + 1) (dropMy start xs)

    strReplaceMy :: Eq a => [a] -> [a] -> [a] -> [a]
    strReplaceMy _ _ [] = []
    strReplaceMy find replace source
        | take len source == find = replace ++ strReplaceMy find replace (drop len source)
        | otherwise = head source : strReplaceMy find replace (drop 1 source)
            where
                len = length find

    --Находит, под какими индексами в списке встречается заданный элемент.
    elemIndices :: Eq a => a -> [a] -> [Int]
    elemIndices _ [] = [] 
    elemIndices y xs = elemIndicesTwo y xs 0
        where
            elemIndicesTwo :: Eq a => a -> [a] -> Int -> [Int]
            elemIndicesTwo _ [] _ = [] 
            elemIndicesTwo y (x:xs) index
                | y == x = index : elemIndicesTwo y xs (index + 1) 
                | otherwise = elemIndicesTwo y xs (index + 1)

    --Находит все вхождения первого списка во второй и возвращает список номеров элементов, с которых эти вхождения начинаются 
    strPosMy :: Eq a => [a] -> [a] -> [Int]
    strPosMy _ [] = []
    strPosMy first second = strPosTwo first second 0
        where
            strPosTwo :: Eq a => [a] -> [a] -> Int -> [Int]
            strPosTwo first second index
                | length second < length first = []
                | take (length first) second == first = index : strPosTwo first (drop 1 second) (index + 1)
                | otherwise = strPosTwo first (drop 1 second) (index + 1)

    strRotateMy :: [a] -> Int -> [a]
    strRotateMy [] x = []
    strRotateMy xs 0 = xs
    strRotateMy xs n = strRotateMy (last xs : init xs) (n-1)

    unevenHandWritingMy :: String -> String
    unevenHandWritingMy [] = []
    unevenHandWritingMy (x:xs) = x : unevenHandWritingMyTwo xs 1
        where
            unevenHandWritingMyTwo :: String -> Int -> String
            unevenHandWritingMyTwo [] _ = []
            unevenHandWritingMyTwo (y:ys) n
                | mod n 3 == 0 && isLower y = toUpper y : unevenHandWritingMyTwo ys (n+1)
                | mod n 3 == 0 && isUpper y = toLower y : unevenHandWritingMyTwo ys (n+1)
                | otherwise = y : unevenHandWritingMyTwo ys (n+1)

    main = do
        let sumMyTest = sumMy 1 2
        let productMyTest = productMy 1 2
        let maxMyTest = maxMy 1 2
        let minMyTest = minMy 1 2
        let maximumMyTest = maximumMy [1, 2, 3]
        let minimumMyTest = minimumMy [1, 2, 3]
        let evenMyTest = evenMy 3
        let oddMyTest = oddMy 3
        let gcdMyTest = gcdMy 3 9
        let lcmMyTest = lcmMy 3 9
        let powerMyTest = powerMy 2 3
        
        let factorialOf5 = factMy 5
        let fibonacciOf8 = fibMy 8
        let fibonacciOf8' = fibMy' 8 1 0

        let andMyTest = andMy [True, True, False]
        let orMyTest = orMy [True, False, False]

        let headMyTest = headMy [1, 2, 3]
        let tailMyTest = tailMy [1, 2, 3]
        let lastMyTest = lastMy [1, 2, 3]
        let initMyTest = initMy [1, 2, 3]
        let lengthMyTest = lengthMy [1, 2, 3]
        let indexMyTest = indexMy [1, 2, 3] 2
        let concatMyTest = concatMy [1, 2] [3]
        let takeMyTest = takeMy 1 [1, 2, 3]
        let dropMyTest = dropMy 1 [1, 2, 3]
        let reverseMyTest = reverseMy [1, 2, 3]
        let elemMyTest = elemMy 1 [1, 2, 3]
        let replicateMyTest = replicateMy 1 'a'

        let lookupMyTest = lookupMy 1 2 [(3, 2), (4, 3), (1, 2)]
        let substrMyTest = substrMy "Amogus" 3 5
        let strReplaceMyTest = strReplaceMy "Amog" "s" "This is a Amogus"
        let elemIndicesTest = elemIndices 2 [2, 3, 2]
        let strPosMyTest = strPosMy [1, 2] [1, 2, 3, 1, 2]
        let strRotateMyTest = strRotateMy [1, 2, 3, 4, 5, 6] 2
        let unevenHandWritingMyTest = unevenHandWritingMy "my name is walter white"

        print "simplest"
        print sumMyTest
        print productMyTest
        print maxMyTest
        print minMyTest
        print maximumMyTest
        print minimumMyTest
        print evenMyTest
        print oddMyTest
        print gcdMyTest
        print lcmMyTest
        print powerMyTest

        print "recursive"
        print factorialOf5
        print fibonacciOf8
        print fibonacciOf8'

        print "logical"
        print andMyTest
        print orMyTest

        print "lists"
        print headMyTest
        print tailMyTest
        print lastMyTest
        print initMyTest
        print lengthMyTest
        print indexMyTest
        print concatMyTest
        print takeMyTest
        print dropMyTest
        print reverseMyTest
        print elemMyTest
        print replicateMyTest

        print "lists 2"

        print lookupMyTest
        print substrMyTest
        print strReplaceMyTest
        print elemIndicesTest
        print strPosMyTest
        print strRotateMyTest
        print unevenHandWritingMyTest