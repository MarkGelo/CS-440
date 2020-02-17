-- CS 440 Lecture 7, Spring 2020
-- (skeleton)

-- Regular expressions
data RegExpr a
    = RE_const a
    | RE_or [RegExpr a]
    | RE_and [RegExpr a]
    | RE_star (RegExpr a) 
    | RE_any                  -- dot   - any one symbol
    | RE_in_set [a]           -- [...] - any symbol in list 
    | RE_end                  -- $  - at end of input
    | RE_empty                -- epsilon - empty (no symbols)
        deriving (Eq, Read, Show)

match :: Eq a => RegExpr a -> [a] -> Maybe [a]

match (RE_const _) [] = Nothing
match (RE_const symbol) (head_inp : input')
    | head_inp == symbol = Just input'
    | otherwise = Nothing

match (RE_or []) _ = Nothing            -- the OR of no clauses fails
match (RE_or (rexp : regexprs')) input =
    case match rexp input of
        Nothing -> match (RE_or regexprs') input
        ok @ (Just _) -> ok

match (RE_and []) input = Just input    -- the AND of no clauses succeeds
match (RE_and (rexp : regexprs)) input =
    case match rexp input of
        Nothing -> Nothing
        ok @ (Just input') -> match (RE_and regexprs) input'

-- other cases omitted in skeleton
