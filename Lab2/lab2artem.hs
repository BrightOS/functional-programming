
module Huggie where
import Data.Char
import Data.List (sort, group, sortBy)
import Debug.Trace


digitToIntMy :: Char -> Int
digitToIntMy c
  | isDigit c            =  ord c - ord '0'
  | c >= 'a' && c <= 'f' =  ord c - ord 'a' + 10
  | c >= 'A' && c <= 'F' =  ord c - ord 'A' + 10
  | otherwise            =  error "My error"


intToDigitMy :: Int -> Char
intToDigitMy i
  | i >= 0  && i <=  9   =  chr (ord '0' + i)
  | i >= 10 && i <= 15   =  chr (ord 'A' + i - 10)
  | otherwise            =  error "My error"


hexToDecMy :: String -> Int
hexToDecMy x = (hexToDecHelper x (length x - 1))
	where
       	    hexToDecHelper [] _          =   0
            hexToDecHelper (x : xs) n    =   digitToIntMy x * (16 ^ n) + (hexToDecHelper xs (n-1))


decToHexMy :: Int -> String
decToHexMy x = aux x []
	where
       	    aux n l 
	        |   n < 16      = [intToDigitMy n] ++ l
                |   otherwise   = aux (div n 16)  ([intToDigitMy (mod n 16)] ++ l)


zipMy :: [a] -> [b] -> [(a, b)]
zipMy [] _ = []
zipMy _ [] = []
zipMy (x:xs) (y:ys) = (x, y) : zipMy xs ys


unzipMy :: [(a, b)] -> ([a], [b])
unzipMy [] = ([], [])
unzipMy ((x, y):xs) = (x : fst (unzipMy xs), y : snd (unzipMy xs))


nub :: Ord a => [a] -> [a]
nub [] = []				 --Список пуст - возвращаем пустой
nub (x:xs) = if elem x xs then nub xs else x : nub xs


delete :: Eq a => a -> [a] -> [a]
delete _ [] = []  			 -- Если список пуст, возвращаем пустой список
delete x (y:ys)
    | x == y    = ys       		 -- Если нашли элемент, то возвращаем оставшуюся часть списка
    | otherwise = y : delete x ys  	 -- Иначе продолжаем поиск с остальной частью списка


union :: Eq a => [a] -> [a] -> [a]
union xs [] = xs 			 -- 1: если второй список пуст, возвращаем первый
union [] ys = ys 			 -- 2: если первый список пуст, возвращаем второй
union (x:xs) ys
    | elem x ys = union xs ys 		 -- Если элемент x уже есть в ys, просто продолжаем с остальными элементами xs
    | otherwise   = x : union xs ys	 -- Иначе добавляем x к результату объединения остатка xs и всего списка ys


(\\) :: Eq a => [a] -> [a] -> [a]
(\\) [] _ = []  			 -- 1: если второй список пуст, возвращаем первый
(\\) xs [] = xs 			 -- 2: если первый список пуст, возвращаем второй
(\\) (x:xs) ys
    | elem x ys = (\\) xs ys		 --Eсли x уже есть в ys, то продолжаем с остальными элементами xs
    | otherwise = x : (\\) xs ys	 --Иначе добавляем x к результату вычитания остатка xs и всего списка ys


intersect :: Eq a => [a] -> [a] -> [a]
intersect [] _ = []			 -- 1: если второй список пуст, возвращаем первый
intersect _ [] = []			 -- 2: если первый список пуст, возвращаем второй
intersect (x:xs) ys	
  | elem x ys = x : intersect xs ys      -- Eсли x уже есть в ys, то добавляем x к результату и продолжаем дальше
  | otherwise = intersect xs ys		 -- Иначе продолжаем сравнние



insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]			 -- Если список пуст - вставляем х
insert x (y:ys)
  | x < y = x : y : ys			 -- Как только нашли Первый элемент <= x - вставляем х в список
  | otherwise = y : insert x ys		 -- Иначе продолжаем


sortMy :: Ord a => [a] -> [a]
sortMy [] = []				 -- Если список пуст - возвращаем пустой
sortMy (x:xs) = insert x (sortMy xs)


countCharsMy :: String -> [(Char, Int)]
countCharsMy xs = sortByFrequencyCount $ map (\x -> (head x, length x)) $ group $ sort xs
  where
    sortByFrequencyCount = sortBy (flip compare `on` snd) where
	on f g a b = f (g a) (g b)


qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a < x]
                      ++ [x] ++
               qsort [b | b <- xs, b >= x]