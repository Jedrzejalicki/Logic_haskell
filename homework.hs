import Data.List

is_divisible :: Int -> [Int] -> Bool
is_divisible n [] = False
is_divisible n (x:xs) 
        | mod n x == 0 = True
        | otherwise = is_divisible n xs
-- checking if is divisble by element in list


sum_of :: Int -> [Int] -> Int
sum_of n l = sum([x | x <- [1..n], is_divisible x l])
-- is is divisible summing up

     

data Sentence = S Char | N Sentence | I Sentence Sentence | C Sentence Sentence | A Sentence Sentence
              deriving (Eq)
-- making new data type sentence 

instance Show Sentence where
    show(S p) = [p]
    show(N p) = "~" ++ show p
    show(I p q) = "(" ++ show p ++ " => " ++ show q ++ ")"
    show(A q p) = "(" ++ show p ++ " | " ++ show q ++ ")"
    show(C p q) = "(" ++ show p ++ " & " ++ show q ++ ")"
-- function print_sentence, using instance of show and recursivlu calling every function (A,I,C,N)

get_variables :: Sentence -> [Char]
get_variables(S p) = [p]
get_variables(N p) =  get_variables p
get_variables(I p q) = get_variables p ++ get_variables q 
get_variables(A p q) = get_variables p ++ get_variables q 
get_variables(C p q) = get_variables p ++ get_variables q 

-- getting variables recursivly from every operation and saving them as [Char]







get_rid_of_repetition :: [[Char]] -> [[Char]]
get_rid_of_repetition [] = []
get_rid_of_repetition (x:xs) 
                  | elem x xs = get_rid_of_repetition xs
                  | otherwise = x:get_rid_of_repetition xs
-- getting rid of repetition in list of char recursivly by checking if head is in tail

write_variables :: Sentence -> [[Char]]
write_variables sentence = sort (get_rid_of_repetition (map(:[]) (get_variables sentence)))
-- making a sorted list without repeptition from String using function map()


create_variables :: Sentence-> [Bool]-> [([Char], Bool)]
create_variables sentence list = zip (write_variables sentence) list
-- making a list of tuples 

from_list :: [(Char, Bool)]-> Char -> Bool
from_list (x:xs) p 
      | p == fst x = snd x 
      | otherwise = from_list xs p
-- comparing first element from head and if it is match then getting second element from tuple, recursivly repeating until found

check_sentence :: Sentence ->[(Char, Bool)] -> Bool
check_sentence(S p) values_map = from_list values_map p
check_sentence(N p) values_map = not (check_sentence p values_map)
check_sentence(I p q) values_map = if (check_sentence p values_map) then (check_sentence q values_map) else True 
check_sentence(A p q) values_map = (check_sentence p values_map) && (check_sentence q values_map)
check_sentence(C p q) values_map = (check_sentence p values_map) || (check_sentence q values_map)
-- recursivly making logic operations 