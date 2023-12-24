module Lab3.Third where
    main :: IO ()

    -- Реализация функции mapMy
    mapMy :: (a -> b) -> [a] -> [b]
    mapMy f [] = []
    mapMy f (x:xs) = f x : mapMy f xs

    -- Реализация функции filterMy
    filterMy :: (a -> Bool) -> [a] -> [a]
    filterMy p [] = []
    filterMy p (x:xs)
        | p x = x : filterMy p xs
        | otherwise = filterMy p xs

    -- Реализация функции anyMy
    anyMy :: (a -> Bool) -> [a] -> Bool
    anyMy p [] = False
    anyMy p (x:xs) = p x || anyMy p xs

    -- Реализация функции allMy
    allMy :: (a -> Bool) -> [a] -> Bool
    allMy p [] = True
    allMy p (x:xs) = p x && allMy p xs

    -- Удаление согласных букв из строки
    removeConsonants :: String -> String
    removeConsonants = filterMy (`notElem` "bcdfghjklmnpqrstvwxyz")

    -- Удаление цифр из строки
    removeDigits :: String -> String
    removeDigits = filterMy (`notElem` "0123456789")

    -- Получение строки из первых букв каждой строки в списке
    firstLetters :: [String] -> String
    firstLetters = mapMy head

    -- Получение списка из последних элементов подсписков
    lastElements :: [[a]] -> [a]
    lastElements = mapMy last

    -- Получение списка из N-х элементов подсписков
    getNthElements :: [[a]] -> Int -> [a]
    getNthElements lists n = mapMy (!! n) lists

    -- Обращение списка и его подсписков
    reverseAll :: [[a]] -> [[a]]
    reverseAll = mapMy reverse

    -- Условное применение функций к элементам списка
    mapIfMy :: (a -> Bool) -> (a -> b) -> (a -> b) -> [a] -> [b]
    mapIfMy cond f1 f2 xs = mapMy (\x -> if cond x then f1 x else f2 x) xs

    -- Последовательная композиция функций
    composeAllMy :: [a -> a] -> (a -> a)
    composeAllMy = foldr (.) id

    -- Применение функций последовательно к значению
    applyIterateMy :: [a -> a] -> a -> a
    applyIterateMy fs x = foldr (\f acc -> f acc) x fs

    -- Разделение списка на элементы, удовлетворяющие и не удовлетворяющие условию
    partition :: (a -> Bool) -> [a] -> ([a], [a])
    partition p xs = (filter p xs, filter (not . p) xs)

    -- Количество предикатов, возвращающих True для элемента
    countTruePredicates :: [a -> Bool] -> a -> Int
    countTruePredicates preds x = length (filter (\p -> p x) preds)

    -- Индексы элементов, удовлетворяющих условию
    findIndices :: (a -> Bool) -> [a] -> [Int]
    findIndices p xs = [i | (x, i) <- zip xs [0..], p x]

    -- Сортировка элементов в списке с использованием сортировочной функции
    sortBy :: (a -> a -> Ordering) -> [a] -> [a]
    sortBy cmp = foldr insert []
        where 
            insert x [] = [x]
            insert x (y:ys)
                | cmp x y == GT = y : insert x ys
                | otherwise = x : y : ys

    -- Комбинирование функций с использованием бинарной функции
    on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
    on fb fa x y = fb (fa x) (fa y)

    -- Фильтрация элементов, на которых все предикаты возвращают True
    filterMapAndMy :: [a -> Bool] -> [a] -> [a]
    filterMapAndMy preds xs = filter (\x -> all (\p -> p x) preds) xs

    -- Фильтрация элементов, на которых хотя бы один предикат возвращает True
    filterMapOrMy :: [a -> Bool] -> [a] -> [a]
    filterMapOrMy preds xs = filter (\x -> any (\p -> p x) preds) xs

    -- Сложение списков чисел "столбиком"
    sumEqMy :: [[Int]] -> [Int]
    sumEqMy lists = foldl1 (zipWith (+)) paddedLists
        where
            maxLen = maximum (map length lists)
            paddedLists = map (addZeros maxLen) lists

            addZeros :: Int -> [Int] -> [Int]
            addZeros n xs
                | length xs >= n = xs
                | otherwise = xs ++ replicate (n - length xs) 0

    -- Применение функции к каждому элементу списка с накоплением состояния
    mapAccumLMy :: (acc -> x -> (acc, y)) -> acc -> [x] -> (acc, [y])
    mapAccumLMy f acc xs = foldl (\(acc', ys) x -> let (acc'', y) = f acc' x in (acc'', ys ++ [y])) (acc, []) xs

    -- Разделение списка функций на те, что возвращают True и False для заданного значения
    segregateFMy :: [a -> Bool] -> a -> ([a -> Bool], [a -> Bool])
    segregateFMy funcs value = (filter (\f -> f value) funcs, filter (\f -> not (f value)) funcs)

    -- Функция из списка [Integer] возвращает все возможные пары чисел, где первое число меньше второго
    integerPairs :: [Integer] -> [(Integer, Integer)]
    integerPairs xs = [(x, y) | x <- xs, y <- xs, x < y]

    -- Аналогичная функция без использования генераторов списков
    integerPairs' :: [Integer] -> [(Integer, Integer)]
    integerPairs' xs = concatMap (\x -> map (\y -> (x, y)) (filter (>x) xs)) xs


    main = do
        let mapTest = map (\x -> x * 2) [1, 2, 3]
        let filterTest = filter (>0) [-1, 1]
        let zipWithTest = zipWith (*) [1, 2, 3] [3, 2, 1]
        let dotTest = (filter (\x -> x `mod` 2 == 0) . map (\x -> x * 2)) [-1, 1, 2, 3]
        let dollarTest = filter (\x -> x `mod` 2 == 0) $ map (\x -> x * 2) [-1, 1, 2, 3]
        let flipTest = flip (/) 1 2
        let idTest = id "12314314"
        let constTest = const 123 142
        let concatMapTest = concatMap (\x -> [x * 2]) [1, 2, 3]

        let foldlTest = foldl (/) 4 [2, 2]
        let foldl1Test = foldl1 (/) [4, 2, 2]
        let scanlTest = scanl (/) 4 [2, 2]
        let scanl1Test = scanl1 (/) [4, 2, 2]

        let lastElementsTest = lastElements [[1, 2], [3, 2], [3, 3]]
        let sumEqMyTest = sumEqMy [[1, 2, 3, 4, 5], [4, 5, 6, 7]]
        let integerPairsTest = integerPairs [1, 2, 9, 3, 5]
        let integerPairs'Test = integerPairs' [1, 2, 9, 3, 5]
    
        print mapTest
        print filterTest
        print zipWithTest
        print dotTest
        print dollarTest
        print flipTest
        print idTest
        print constTest
        print concatMapTest

        print foldlTest
        print foldl1Test
        print scanlTest
        print scanl1Test

        print lastElementsTest
        print sumEqMyTest
        print integerPairsTest
        print integerPairs'Test