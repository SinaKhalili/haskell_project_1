import System.Environment
import System.IO
import Data.Bits

-- Different ways of storing a regular expression 
data StateNode = StateNode { ch :: Char, state :: Integer} deriving Show

data RE = Epsilon | Ch Char Integer | Seq RE RE | Alt RE RE | Star RE | Group RE deriving Show
-- Different states
-- Linearize defintions

linearize :: RE -> Integer -> (RE , Integer)

--Base case for when we hit a Ch a _ 

linearize (Ch a _) c  = let counter = c + 1
                            in ((Ch a counter), counter)

linearize (Seq r1 r2) c = let (left, c1) = linearize r1 c
                              (right, c2) = linearize r2 c1
                             in (Seq left right, c2)

linearize (Alt r1 r2) c = let (left, c1) = linearize r1 c
                              (right, c2) = linearize r2 c1
                             in (Alt left right, c2)

linearize (Star r1) c = let (left, c1) = linearize r1 c
                             in (Star left , c1)

linearize (Group r1) c = let (left, c1) = linearize r1 c
                             in (Group left , c1)

-- End linearize 


-- 

--Now we linearize, and give an in-order array of state nodes?
linearizeToNodes :: RE -> Integer -> [StateNode]

linearizeToNodes (Ch a _) c = let counter = c + 1
                     in [StateNode {ch = a, state = counter}]
linearizeToNodes (Seq r1 r2) c = let (leftNodeArray) = linearizeToNodes r1 c;
                                     (rightNodeArray) = linearizeToNodes r2 (state (last leftNodeArray))
                              in leftNodeArray ++ rightNodeArray

linearizeToNodes (Alt r1 r2) c = let (left) = linearizeToNodes r1 c
                                     (right) = linearizeToNodes r2 (state (last left))
                              in left ++ right

linearizeToNodes (Star r1) c = let (left) = linearizeToNodes r1 c
                            in left
linearizeToNodes (Group r1) c = let (left) = linearizeToNodes r1 c
                            in left

---- now to turn them into the bitmask/vector versions

vectorize :: [StateNode] -> [StateNode]
vectorize [] = []
vectorize (a:more) = [StateNode {ch = (ch a), state = 2^(state a)}] ++ vectorize more

vectorizeTuple :: [(StateNode, StateNode)] -> [(StateNode, StateNode)]
vectorizeTuple [] = []
vectorizeTuple ((a,b):more) = [(StateNode {ch = (ch a), state = 2^(state a)}, StateNode {ch = (ch b), state = 2^(state b)})] ++ vectorizeTuple more


--Start states
find_start_states :: RE -> [StateNode]

find_start_states (Ch a b) = [StateNode {ch = a, state = b}]
-- find_start_states (Alt r1 r2) = [r1, r2] >>= find_start_states
find_start_states (Alt r1 r2) = let (left) = find_start_states r1
                                    (right) = find_start_states r2
                                   in  left ++ right
find_start_states (Seq (Star r1) r2) = let (left) = find_start_states r1
                                           (right) = find_start_states r2
                                          in  left ++ right
find_start_states (Seq Epsilon r2) = find_start_states r2
find_start_states (Seq r1 _) = find_start_states r1
find_start_states (Star r1) = find_start_states r1
find_start_states (Group r1) = find_start_states r1

-- Final States 
find_final_states :: RE -> [StateNode]

find_final_states (Ch a c) = [StateNode {ch = a, state = c}]

find_final_states (Alt r1 r2) = let (left) = find_final_states r1
                                    (right) = find_final_states r2
                                   in left ++ right
find_final_states (Seq r1 (Star r2)) = let (left) = find_final_states r1
                                           (right) = find_final_states r2
                                          in left ++ right
find_final_states (Seq Epsilon r2) = find_final_states r2
find_final_states (Seq _ r2) = find_final_states r2
find_final_states (Star r1) = find_final_states r1
find_final_states (Group r1) = find_final_states r1
                                
--Pair states
--makes pairs 
makepairs :: [a] -> [a] -> [(a, a)]
makepairs x y = [(a, b) | a <- x, b <- y]
--Merges two RE and a pair , it strictly mergess the first RE since in Alt we only want the pair r1,r2 we cannot have r2,r1
merge_exclusive_pairs :: RE -> RE -> [(StateNode, StateNode)] -> [(StateNode, StateNode)]
merge_exclusive_pairs r1 r2 nodes = let (left) = find_pair_states r1 nodes
                                        (right) = find_pair_states r2 []
                                       in left ++ right

-- REcursivley finds the pairs
find_pair_states :: RE -> [(StateNode, StateNode)] -> [(StateNode, StateNode)]
find_pair_states (Ch _ _) nodes = nodes
find_pair_states (Alt r1 r2) nodes = merge_exclusive_pairs r1 r2 nodes
find_pair_states (Seq r1 r2) nodes = let (left) = find_start_states r1
                                         (right) = find_final_states r2
                                         (pairs) = makepairs left right
                                         (exclusive_pairs) = merge_exclusive_pairs r1 r2 nodes
                                       in exclusive_pairs ++ pairs
find_pair_states (Star r1) nodes = let (left) = find_start_states r1
                                       (right) = find_final_states r1
                                       (pairs) = makepairs left right
                                      in (find_pair_states r1 pairs)
find_pair_states (Group r1) nodes = find_pair_states r1 nodes
--  Functions to merge a bunch of states into a single bitmask


find_merged_states_num_bin :: [StateNode] -> Integer

find_merged_states_num_bin [] = 0
find_merged_states_num_bin (h:xs) = state h .|. find_merged_states_num_bin xs

find_merged_states_num_bin_helper :: [StateNode] -> Integer
find_merged_states_num_bin_helper xs = find_merged_states_num_bin xs

--- D table
is_char_correct_tuple :: Integer ->  (StateNode, StateNode) -> Bool
is_char_correct_tuple b a = (state (fst a)) == b

get_D_table_entry :: [(StateNode, StateNode)] -> Integer -> Integer
get_D_table_entry xs c = (find_merged_states_num_bin_helper (map snd  (filter (is_char_correct_tuple c) xs)))


--  B table

is_char_correct :: Char -> StateNode -> Bool
is_char_correct b a = (ch a) == b 

get_B_table_entry :: [StateNode] -> Char -> Integer
get_B_table_entry xs c = find_merged_states_num_bin_helper $ filter (is_char_correct c) xs  

-----------------------------------------------------
--- Regex Matching now



-- 3.  A parser to convert text into regular expressions

parseRE :: [Char] -> Maybe (RE, [Char])
parseSeq :: [Char] -> Maybe (RE, [Char])
parseItem :: [Char] -> Maybe (RE, [Char])
parseElement :: [Char] -> Maybe (RE, [Char])
parseChar :: [Char] -> Maybe (RE, [Char])

parseChar [] = Nothing
parseChar ('\\':c:s) = Just ((Ch c 0), s)
parseChar (c:s)
  | c == '|' || c == '*' || c == '(' || c == ')'   = Nothing
  | otherwise                                      = Just ((Ch c 0), s)

parseElement ('(':more) =
    case parseRE(more) of
        Just (re, ')':yet_more) -> Just(Group re, yet_more)
        _ -> Nothing
parseElement s = parseChar s

parseItem s =
   case parseElement(s) of
        -- Just (re, '\\':c:s) -> Just ((Ch c), s)  
        Just (re, '*':more) -> Just (Star re, more)
        Just (re, more) -> Just (re, more)
        _ -> Nothing

extendSeq :: (RE, [Char]) -> Maybe (RE, [Char])

parseSeq s =
    case parseItem(s) of
        Just (r, more_chars) -> extendSeq(r, more_chars)
        _ -> Nothing

extendSeq (e1, after1) =
    case parseItem(after1) of 
        Just(e2, more) -> extendSeq(Seq e1 e2, more)
        _ -> Just(e1, after1)

extendRE :: (RE, [Char]) -> Maybe (RE, [Char])
parseRE s =
    case parseSeq(s) of
        Just (r, more_chars) -> extendRE(r, more_chars)
        _ -> Nothing

extendRE (e1, []) = Just (e1, [])
extendRE (e1, '|' : after_bar) =
    case parseSeq(after_bar) of 
        Just(e2, more) -> extendRE(Alt e1 e2, more)
        _ -> Nothing
extendRE(e1, c:more) = Just (e1, c:more)

parseMain :: [Char] -> Maybe RE

parseMain s = case parseRE s of 
    Just (e, []) -> Just e
    _ -> Nothing

match :: RE -> [Char] -> Bool
match re [] = False
match re (a:lines) = char_by_char_traversal_helper re (a:lines) .|. char_by_char_traversal_helper re lines
-- find the start states find_merged_states_num_bin(vectorize (find_start_states (fst (linearize re 0))))




char_by_char_traversal :: RE -> [Char] -> Integer -> [(StateNode, StateNode)] -> Integer -> Integer -> Integer
char_by_char_traversal re (c:lines) startStates pairs endStates currentState
    | null lines        = ((get_D_table_entry (vectorizeTuple pairs) currentState) .|. startStates) .&. (get_B_table_entry (vectorize (linearizeToNodes re 0)) c)
    | currentState == 1 = let stateCalculation = startStates .&. (get_B_table_entry (vectorize (linearizeToNodes re 0)) c)
                              in char_by_char_traversal re lines startStates pairs endStates stateCalculation
    | otherwise         = let stateCalculation = ((get_D_table_entry (vectorizeTuple pairs) currentState) .|. startStates) .&. (get_B_table_entry (vectorize (linearizeToNodes re 0)) c)
                              in char_by_char_traversal re lines startStates pairs endStates stateCalculation

char_by_char_traversal_helper :: RE -> [Char] -> Bool
char_by_char_traversal_helper re lines = 
                                          let (startStates) = find_merged_states_num_bin(vectorize (find_start_states (fst (linearize re 0))));
                                               (endStates) = find_merged_states_num_bin(vectorize (find_final_states (fst (linearize re 0))))
                                              (currentState) = 1
                                             in good_final_state (char_by_char_traversal re lines startStates (find_pair_states (fst (linearize re 0)) []) endStates currentState) endStates

-- 4.  Searching for matching lines in a file

-- matches :: RE -> [[Char]] -> [[Char]]
-- matches re lines = filter (match re) lines

-- matching :: [Char] -> [[Char]] -> [[Char]]
-- matching regexp lines = case parseMain regexp of
--                             Just r -> matches r lines
--                             _ -> []


good_final_state :: Integer -> Integer -> Bool
good_final_state a b = a .&. b /= 0


matches :: RE -> [[Char]] -> [[Char]]
matches re lines = filter (match re) lines 

-- matches :: RE -> [[Char]] -> [[Char]]
-- matches re lines = filter (match re) lines

matching :: [Char] -> [[Char]] -> [[Char]]
matching regexp lines = case parseMain regexp of
                            Just r -> matches r lines
                            _ -> []

--- general purpose parser

-- gmatching :: [Char] -> RE
-- gmatching regexp  = case parseMain regexp of
--                             Just r ->  r 
--                             _ -> Epsilon




 -- 5.  Command line interface
main = do
  [regExp, fileName] <- getArgs
  srcText <- readFile fileName
  hPutStr stdout (unlines (matching regExp (lines srcText)))
