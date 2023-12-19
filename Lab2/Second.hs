{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
module Second where

    import Data.Char

    main :: IO ()

    digitToIntMy :: Char -> Int
    digitToIntMy c
        | isDigit c = ord c - ord '0'
        | c >= 'a' && c <= 'f' =  ord c - ord 'a' + 10
        | c >= 'A' && c <= 'F' =  ord c - ord 'A' + 10
        | otherwise = error (c : " - Not a digit")

    intToDigitMy :: Int -> Char
    intToDigitMy n
        | n >= 0 && n <= 9 = chr (ord '0' + n)
        | otherwise = error "Not in range 0-9"

    hexToDecMy :: String -> Int
    hexToDecMy x = (hexToDecMy' x (length x - 1))
        where
            hexToDecMy' [] _ = 0
            hexToDecMy' (x : xs) n = digitToIntMy x * (16 ^ n) + (hexToDecMy' xs (n-1))

    decToHexMy :: Int -> String
    decToHexMy x = decToHexMy' x []
        where
            decToHexMy' n l 
                | n < 16 = [intToDigitMy n] ++ l
                | otherwise = decToHexMy' (div n 16)  ([intToDigitMy (mod n 16)] ++ l)

    romanToArabMy :: String -> Integer
    romanToArabMy romanNumeral = romanToArabMy' romanNumeral 0
        where
            romanToArabMy' :: String -> Integer -> Integer
            romanToArabMy' "" acc = acc
            romanToArabMy' (x:xs) acc
                | x == 'I' = case xs of
                                'V':rest -> romanToArabMy' rest (acc + 4)
                                'X':rest -> romanToArabMy' rest (acc + 9)
                                _        -> romanToArabMy' xs (acc + 1)
                | x == 'V' = romanToArabMy' xs (acc + 5)
                | x == 'X' = case xs of
                                'L':rest -> romanToArabMy' rest (acc + 40)
                                'C':rest -> romanToArabMy' rest (acc + 90)
                                _        -> romanToArabMy' xs (acc + 10)
                | x == 'L' = romanToArabMy' xs (acc + 50)
                | x == 'C' = case xs of
                                'D':rest -> romanToArabMy' rest (acc + 400)
                                'M':rest -> romanToArabMy' rest (acc + 900)
                                _        -> romanToArabMy' xs (acc + 100)
                | x == 'D' = romanToArabMy' xs (acc + 500)
                | x == 'M' = romanToArabMy' xs (acc + 1000)
                | otherwise = romanToArabMy' xs acc

    arabToRomanMy :: Integer -> String
    arabToRomanMy num
        | num >= 1000 = 'M' : arabToRomanMy (num - 1000)
        | num >= 900 = 'C' : 'M' : arabToRomanMy (num - 900)
        | num >= 500 = 'D' : arabToRomanMy (num - 500)
        | num >= 400 = 'C' : 'D' : arabToRomanMy (num - 400)
        | num >= 100 = 'C' : arabToRomanMy (num - 100)
        | num >= 90 = 'X' : 'C' : arabToRomanMy (num - 90)
        | num >= 50 = 'L' : arabToRomanMy (num - 50)
        | num >= 40 = 'X' : 'L' : arabToRomanMy (num - 40)
        | num >= 10 = 'X' : arabToRomanMy (num - 10)
        | num >= 9 = 'I' : 'X' : arabToRomanMy (num - 9)
        | num >= 5 = 'V' : arabToRomanMy (num - 5)
        | num >= 4 = 'I' : 'V' : arabToRomanMy (num - 4)
        | num >= 1 = 'I' : arabToRomanMy (num - 1)
        | otherwise = ""    

    zipMy :: [a] -> [b] -> [(a, b)]
    zipMy [] _ = []
    zipMy _ [] = []
    zipMy (x:xs) (y:ys) = (x, y) : zipMy xs ys

    unzipMy :: [(a, b)] -> ([a], [b])
    unzipMy [] = ([], [])
    unzipMy ((x, y):xs) = (x : fst (unzipMy xs), y : snd (unzipMy xs))

    -- Удаление дубликатов
    nubMy :: Ord a => [a] -> [a]
    nubMy [] = []
    nubMy (x:xs)
        | elem x xs = nubMy xs
        | otherwise = x : nubMy xs

    -- Удаляет первое вхождение
    deleteMy :: Eq a => a -> [a] -> [a]
    deleteMy _ [] = []
    deleteMy x (y:ys)
        | x == y = ys
        | otherwise = y : deleteMy x ys

    -- Объединяет списки как множества
    unionMy :: Eq a => [a] -> [a] -> [a]
    unionMy xs [] = xs
    unionMy [] ys = ys
    unionMy (x:xs) ys
        | elem x ys = unionMy xs ys
        | otherwise = x : unionMy xs ys

    -- Разность списков как множеств
    (\\\) :: Eq a => [a] -> [a] -> [a]
    (\\\) [] _ = []
    (\\\) xs [] = xs
    (\\\) (x:xs) ys
        | elem x ys = (\\\) xs ys
        | otherwise = x : (\\\) xs ys

    -- Пересечение списков как множеств
    intersectMy :: Eq a => [a] -> [a] -> [a]
    intersectMy [] _ = []
    intersectMy _ [] = []
    intersectMy (x:xs) ys
        | elem x ys = x : intersectMy xs ys
        | otherwise = intersectMy xs ys

    sortMy :: Ord a => [a] -> [a]
    sortMy [] = []
    sortMy (x:xs) = insert x (sortMy xs)

    countCharsMy :: String -> [(Char, Int)]
    countCharsMy xs = sortByFrequencyCount $ map (\x -> (head x, length x)) $ group $ sort xs
        where
            sortByFrequencyCount = sortBy (flip (on compare snd)) where
            on f g a b = f (g a) (g b)

    quickSortMy :: (Ord a) => [a] -> [a]
    quickSortMy [] = []
    quickSortMy (x:xs) = quickSortMy [a | a <- xs, a < x]
        ++ [x] ++
        quickSortMy [b | b <- xs, b >= x]

    main = do
        let digitToIntMyTest = digitToIntMy '4'
        let intToDigitMyTest = intToDigitMy 4

        let hexToDecMyTest = hexToDecMy "F"
        let decToHexMyTest = decToHexMy 17

        let arabToRomanMyTest = arabToRomanMy 1984
        let romanToArabMyTest = romanToArabMy "MCMLXXXIV"

        let zipMyTest = zipMy [1, 2, 3] [4, 5, 6]
        let unzipMyTest = unzipMy [(1, 5), (2, 6), (3, 7), (4, 8)]
        let nubMyTest = nubMy [1, 1, 1, 1, 1, 2, 3, 2, 3]
        let deleteMyTest = deleteMy 2 [1, 2, 2, 3]
        let unionMyTest = unionMy [1, 5, 3] [4, 3, 2, 1]
        let differenceMyTest = (\\\) [1, 4, 3, 5, 2] [1, 4, 2]
        let intersectMyTest = intersectMy [1, 2, 4, 3] [2, 1]

        let sortMyTest = sortMy [1, 4, 2, 6, 5, 9]
        let countCharsMyTest = countCharsMy "whatthehell"
        let quickSortMyTest = quickSortMy [1, 4, 2, 6, 5, 9]

        print digitToIntMyTest
        print intToDigitMyTest
        print hexToDecMyTest
        print decToHexMyTest
        print arabToRomanMyTest
        print zipMyTest
        print unzipMyTest
        print nubMyTest
        print deleteMyTest
        print unionMyTest
        print differenceMyTest
        print intersectMyTest