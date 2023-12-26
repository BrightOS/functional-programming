module Lab4.Fourth where

    import Data.List
    import Data.Function
    import Data.Char
    import System.IO
    import System.Directory
    import System.FilePath

    main :: IO ()

    iterateMy :: (a -> a) -> a -> [a]
    iterateMy f x = x : iterateMy f (f x)

    repeatMy :: a -> [a]
    repeatMy x = x : repeatMy x
    
    cycleMy :: [a] -> [a]
    cycleMy [] = []
    cycleMy xs = xs ++ cycleMy xs

    -- Реализация функции pseudoRandomMy с использованием линейного конгруэнтного метода
    pseudoRandomMy :: Integer -> [Integer]
    pseudoRandomMy seed = iterate (\x -> (17 * x + 43) `mod` 100) seed

    -- Реализация функции для списка символов, представляющих натуральные числа
    naturalNumbers :: [Char]
    naturalNumbers = concatMap (show . (+ 1)) [0..]

    -- Реализация функции для списка символов, представляющих степени десятки
    powersOfTen :: [Char]
    powersOfTen = concatMap show (iterate (* 10) 1)

    -- Реализация функции для бесконечного списка подсписков чисел
    powerLists :: [[Integer]]
    powerLists = map (\n -> map (\k -> n^k) [1..]) [1..]

    -- Функция для проверки правильности расстановки скобок в тексте
    checkBrackets :: String -> Bool
    checkBrackets text = go text []
        where
            go :: String -> [Char] -> Bool
            go [] stack = null stack
            go (x:xs) stack
                | isOpeningBracket x = go xs (x:stack)
                | isClosingBracket x = not (null stack) && matchBracket (head stack) x && go xs (drop 1 stack)
                | otherwise = go xs stack

            isOpeningBracket :: Char -> Bool
            isOpeningBracket x = x `elem` "({["

            isClosingBracket :: Char -> Bool
            isClosingBracket x = x `elem` ")}]"

            matchBracket :: Char -> Char -> Bool
            matchBracket '(' ')' = True
            matchBracket '{' '}' = True
            matchBracket '[' ']' = True
            matchBracket _ _ = False

    -- Функция для подсчета уникальных слов и их количества в тексте
    -- group: "abbacabba" -> ["a", "bb", "a", "c", "a", "bb", "a"]
    -- flip для обратной сортировки
    wordCount :: String -> [(String, Int)]
    wordCount text = sortBy (flip $ on compare snd) $ map (\group -> (head group, length group)) (group (sort (words text)))

    -- Функция для подсчета вхождений букв в тексте
    letterCount :: String -> [(Char, Int)]
    letterCount text = sortBy (flip $ on compare snd) $ map (\group -> (head group, length group)) (group (sort (filter isLetter text)))

    -- 
    onMy :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    onMy binary unary x y = binary (unary x) (unary y)

    -- Функция для сортировки уникальных слов в тексте без учета регистра
    -- nub убирает дупликаты
    sortUniqueWords :: String -> [String]
    sortUniqueWords text = nub $ sort (map (map toLower) (words text))

    -- Функция для фильтрации строк, содержащих заданное слово
    -- lines - разбиение на массив строк, unlines - reversed lines
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
        first : explodeMy separator (dropWhile (== separator) rest)
            where
                (first, rest) = span (/= separator) list

    -- Функция explodeMy, но со списком сепараторов
    explodeBy :: (a -> Bool) -> [a] -> [([a], [a])]
    explodeBy _ [] = []
    explodeBy f xs = (takeWhile (not . f) xs, takeWhile f (dropWhile (not . f) xs)):
        explodeBy f (dropWhile f (dropWhile (not . f) xs))

    -- Функция с транспонированием строк и столбцов
    transposeMy :: [[a]] -> [[a]]
    transposeMy [] = []
    transposeMy ([]:_) = []
    transposeMy matrix = map head matrix : transposeMy (map (drop 1) matrix)

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

    -- Функция для поиска всех инфиксов
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
    isSuffixOfMy xs ys = (reverse xs) `isPrefixOfMy` (reverse ys)

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

    -- Список количества чисел меньше каждого из элементов
    lessCountMy :: [Int] -> [Int]
    lessCountMy xs = map (\x -> length $ filter (<x) xs) xs

    main = do
        let currentDir = "c:\\funcprog"

        contentsBrackets <- readFile (currentDir </> "brackets.txt")
        putStrLn $ if checkBrackets contentsBrackets then "true" else "false"

        input <- readFile (currentDir </> "input.txt")
        writeFile (currentDir </> "wordcountoutput.txt") (show $ wordCount input)
        writeFile (currentDir </> "lettercountoutput.txt") (show $ letterCount input)
        writeFile (currentDir </> "uniquewordsoutput.txt") (show $ sortUniqueWords input)
        writeFile (currentDir </> "filterbywordoutput.txt") (filterByWord "wow" input)

        let powersOfTenTest = take 6 (powersOfTen)
        let iterateExample = take 5 (iterate (* 2) 1)
        let repeatExample = take 3 (repeat "Hello")
        let cycleExample = take 7 (cycle [1,2,3])

        let explodeByTest = explodeBy (`elem` ".!?") "Something happened... Finally!"
        print explodeByTest

        let transposeMyTest = transposeMy [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
        print transposeMyTest

        let naturalNumbersTest = take 100 naturalNumbers
        let naturalNumbersTest2 = drop 999 $ take 1000 naturalNumbers
        print naturalNumbersTest
        print naturalNumbersTest2

        print powersOfTenTest
        print iterateExample
        print repeatExample
        print cycleExample