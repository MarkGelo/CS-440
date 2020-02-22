-- CS 440 -- extension of lecture 7 RE recognizer to an RE return matched string
-- Spring 2020
-- HW 4 solution for just constants, and, or.

module HW_04_Capture_soln where

-- Regular expressions
data RegExpr a
    = RE_const a
    | RE_or [RegExpr a]
    | RE_and [RegExpr a]
--    | RE_star (RegExpr a) 
--    | RE_any                  -- dot   - any one symbol
--    | RE_in_set [a]           -- [...] - any symbol in list 
--    | RE_end                  -- $  - at end of input
--    | RE_empty                -- epsilon - empty (no symbols)
        deriving (Eq, Read, Show)

-- Give names to Token, reversed token, and Input types so that the
-- type annotations describe some functions better.
--
type Token a = [a]
type RevToken a = [a]
type Input a = [a]

-- capture matches a regular expression against some input; on success,
-- it returns the matching token (= list of input symbols) and the
-- remaining input.  E.g. capture abcd abcdef = Just(abcd, ef)
--
capture  :: Eq a => RegExpr a -> Input a -> Maybe(Token a, Input a)

-- top-level capture routine calls assistant with empty built-up token to start
--
capture rexp input = case capture' rexp ([], input) of
    Nothing -> Nothing
    Just (revtoken, input') -> Just (reverse revtoken, input')

-- capture' rexp (partial_token input) matches the expression against
-- the input given a reversed partial token; on success, it returns
-- the completed token and remaining input. The token is in reverse
-- order. E.g., capture' cd (ba, cdef) = (dcba, ef)
--
capture' :: Eq a => RegExpr a -> (RevToken a, Input a) -> Maybe(RevToken a, Input a)

-- RE_const checks for a given symbol
--
capture' (RE_const _) (_, []) = Nothing
capture' (RE_const symbol) (revtoken, head_inp : input')
    | head_inp == symbol = Just (head_inp : revtoken, input')
    | otherwise = Nothing

-- the OR of no clauses fails; else try regexprs and stop on first success
--
capture' (RE_or []) _ = Nothing
capture' (RE_or (rexp : regexprs')) (revtoken, input) =
    case capture' rexp (revtoken, input) of
        Nothing -> capture' (RE_or regexprs') (revtoken, input)
        ok @ (Just _) -> ok

-- the AND of no clauses succeeds; else try regexprs and fail on first failure
--
capture' (RE_and []) (revtoken, input) = Just (revtoken, input)
capture' (RE_and (rexp : regexprs)) (revtoken, input) =
    case capture' rexp (revtoken, input) of
        Nothing -> Nothing
        Just (revtoken', input')
            -> capture' (RE_and regexprs) (revtoken', input')

-- other regular expression cases weren't required.

