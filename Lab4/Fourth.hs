module Lab4.Fourth where

    import Data.List
    import Data.Function
    import Data.Char

    main :: IO ()
    -- Реализация функции pseudoRandomMy с использованием линейного конгруэнтного метода
    pseudoRandomMy :: Integer -> [Integer]
    pseudoRandomMy seed = iterate (\x -> (17 * x + 43) `mod` 100) seed

    -- Реализация функции для списка символов, представляющих натуральные числа
    naturalNumbers :: [Char]
    naturalNumbers = concatMap (show . (+ 1)) [1..]

    -- Реализация функции для списка символов, представляющих степени десятки
    powersOfTen :: [Char]
    powersOfTen = concatMap show (iterate (* 10) 1)

    -- Реализация функции для бесконечного списка чисел, сумма которых равна их произведению
    sumEqualsProduct :: [Integer]
    sumEqualsProduct = filter (\n -> sumDigits n == productDigits n) [1..]
        where
            sumDigits x = sum (map (\c -> read [c]) (show x))
            productDigits x = product (map (\c -> read [c]) (show x))

    -- Реализация функции для бесконечного списка степеней двойки с нечетной суммой
    oddSumPowersOfTwo :: [Integer]
    oddSumPowersOfTwo = filter (\n -> odd (sumDigits n)) (iterate (* 2) 1)
        where
            sumDigits x = sum (map (\c -> read [c]) (show x))

    -- Реализация функции для бесконечного списка подсписков чисел
    powerLists :: [[Integer]]
    powerLists = map (\n -> map (\k -> n^k) [1..]) [1..]

    -- Функция для проверки правильности расстановки скобок в тексте
    checkBrackets :: String -> Bool
    checkBrackets text = go text []
        where
            go :: String -> [Char] -> Bool
            go [] stack = null stack
            go (c:cs) stack
                | isOpeningBracket c = go cs (c:stack)
                | isClosingBracket c = not (null stack) && matchBracket (head stack) c && go cs (tail stack)
                | otherwise = go cs stack

            isOpeningBracket :: Char -> Bool
            isOpeningBracket c = c `elem` "({["

            isClosingBracket :: Char -> Bool
            isClosingBracket c = c `elem` ")}]"

            matchBracket :: Char -> Char -> Bool
            matchBracket '(' ')' = True
            matchBracket '{' '}' = True
            matchBracket '[' ']' = True
            matchBracket _ _ = False

    -- Функция для подсчета уникальных слов и их количества в тексте
    wordCount :: String -> [(String, Int)]
    wordCount text = sortBy (flip compare `on` snd) $ map (\group -> (head group, length group)) (group (sort (words text)))

    -- Функция для подсчета вхождений букв в тексте
    letterCount :: String -> [(Char, Int)]
    letterCount text = sortBy (flip compare `on` snd) $ map (\group -> (head group, length group)) (group (sort (filter isLetter text)))

    -- Функция для сортировки уникальных слов в тексте без учета регистра
    sortUniqueWords :: String -> [String]
    sortUniqueWords text = nub $ sort (map (map toLower) (words text))

    -- Функция для фильтрации строк, содержащих заданное слово
    filterByWord :: String -> String -> String
    filterByWord word text = unlines $ filter (isInfixOfMy word) (lines text)

    -- Функция для разделения каждого из элементов элементом
    intersperseMy :: a -> [a] -> [a]
    intersperseMy _ [] = []
    intersperseMy _ [x] = [x]
    intersperseMy sep (x:xs) = x : sep : intersperseMy sep xs

    -- Функция для разбиения с сепаратором
    explodeMy :: Eq a => a -> [a] -> [[a]]
    explodeMy _ [] = []
    explodeMy separator list =
        let (first, rest) = span (/= separator) list
        in first : explodeMy separator (dropWhile (== separator) rest)

    -- Функция explodeMy, но со списком сепараторов
    explodeBy :: (a -> Bool) -> [a] -> [([a], [a])]
    explodeBy _ [] = []
    explodeBy f xs = (takeWhile (not . f) xs, takeWhile f (dropWhile (not . f) xs)):
        explodeBy f (dropWhile f (dropWhile (not . f) xs))

    -- Функция с транспонированием строк и столбцов
    transposeMy :: [[a]] -> [[a]]
    transposeMy [] = []
    transposeMy ([]:_) = []
    transposeMy matrix = map head matrix : transposeMy (map tail matrix)

    -- Функция для всевозможных перестановок
    permutationsMy :: [a] -> [[a]]
    permutationsMy [] = [[]]
    permutationsMy (x:xs) = [insertAt i x perm | perm <- permutationsMy xs, i <- [0..length xs]]
        where
            insertAt n y xs = take n xs ++ [y] ++ drop n xs

    -- Функция для группировки одинаковых элементов
    groupMy :: Eq a => [a] -> [[a]]
    groupMy [] = []
    groupMy (x:xs) = (x : takeWhile (== x) xs) : groupMy (dropWhile (== x) xs)

    -- Функция groupMy с операцией сравнения
    groupByMy :: (a -> a -> Bool) -> [a] -> [[a]]
    groupByMy _ [] = []
    groupByMy p (x:xs) = (x : takeWhile (p x) xs) : groupByMy p (dropWhile (p x) xs)

    -- Функция для поиска всех префиксов
    initsMy :: [a] -> [[a]]
    initsMy [] = [[]]
    initsMy (x:xs) = [] : map (x:) (initsMy xs)

    -- Функция для поиска всех суффиксов
    tailsMy :: [a] -> [[a]]
    tailsMy [] = [[]]
    tailsMy list@(x:xs) = list : tailsMy xs

    -- Функция для поиска всех префиксов
    infixesMy :: [a] -> [[a]]
    infixesMy [] = [[]]
    infixesMy list = concatMap (initsMy . tail) (tailsMy list)

    -- Функция для поиска всех непрерывных подсписков
    subsequencesMy :: [a] -> [[a]]
    subsequencesMy [] = [[]]
    subsequencesMy (x:xs) = subsequencesMy xs ++ map (x:) (subsequencesMy xs)

    -- Проверка на префикс
    isPrefixOfMy :: Eq a => [a] -> [a] -> Bool
    isPrefixOfMy [] _ = True
    isPrefixOfMy _ [] = False
    isPrefixOfMy (x:xs) (y:ys) = x == y && isPrefixOfMy xs ys

    -- Проверка на суффикс
    isSuffixOfMy :: Eq a => [a] -> [a] -> Bool
    isSuffixOfMy xs ys = reverse xs `isPrefixOfMy` reverse ys

    -- Проверка на инфикс
    isInfixOfMy :: Eq a => [a] -> [a] -> Bool
    isInfixOfMy xs ys = any (xs `isPrefixOfMy`) (tailsMy ys)

    -- Проверка на подпоследовательность
    isSubsequenceOfMy :: Eq a => [a] -> [a] -> Bool
    isSubsequenceOfMy [] _ = True
    isSubsequenceOfMy _ [] = False
    isSubsequenceOfMy (x:xs) (y:ys) = if x == y then isSubsequenceOfMy xs ys else isSubsequenceOfMy (x:xs) ys

    -- Функция для поиска кумулятивных сумм справа налево
    cumSumPostfixMy :: Num a => [a] -> [a]
    cumSumPostfixMy xs = reverse $ snd $ foldl (\(acc, result) x -> (acc + x, acc + x : result)) (0, []) xs

    -- Функция для поиска кумулятивных сумм слева направо
    cumSumPrefixMy :: Num a => [a] -> [a]
    cumSumPrefixMy xs = snd $ foldl (\(acc, result) x -> (acc + x, acc + x : result)) (0, []) xs

    -- Разница между парами подряд идущих чисел
    diffMy :: [Int] -> [Int]
    diffMy xs = zipWith (-) (tail xs) xs

    -- Функция для преобразования строки с числами и конструкциями "N*K"
    -- в строку с повторением числа N K раз
    foo :: String -> String
    foo = concatMap processToken . words
        where
            processToken :: String -> String
            processToken token =
                case break (== '*') token of
                    (num, '*' : rep) -> replicate (read rep) (read num) ++ " "
                    (num, "") -> num ++ " "
                    _ -> error "Invalid token"

    -- Обратная функция к foo, используя group
    fooReverse :: String -> String
    fooReverse = concatMap (\group -> let (num, rep) = span isDigit group in replicate (length rep) (read num)) . group

    main = do
        let powersOfTenTest = take 6 (powersOfTen)
        let iterateExample = take 5 (iterate (* 2) 1)
        let repeatExample = take 3 (repeat "Hello")
        let cycleExample = take 7 (cycle [1,2,3])

        let explodeByTest = explodeBy (`elem` ".!?") "Something happened... Finally!"
        print explodeByTest

        print powersOfTenTest
        print iterateExample
        print repeatExample
        print cycleExample