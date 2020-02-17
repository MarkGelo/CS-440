-- CS 440 Spring 2020
-- Regular expressions (same as lecture 7's)
data RegExpr a
    = RE_const a
    | RE_or [RegExpr a]
    | RE_and [RegExpr a]
    | RE_star (RegExpr a) 
--    | RE_any                  -- dot   - any one symbol
--    | RE_in_set [a]           -- [...] - any symbol in list 
--    | RE_end                  -- $  - at end of input
--   | RE_empty                -- epsilon - empty (no symbols)
        deriving (Eq, Read, Show)

type Token a = [a]
type RevToken a = [a]
type Input a = [a]

capture :: Eq a => RegExpr a -> Input a -> Maybe(Token a, Input a)
capture' :: Eq a => RegExpr a -> (RevToken a, Input a) -> Maybe(RevToken a, Input a)

capture rexp input =
  case capture' rexp ([], input) of
    Nothing -> Nothing
    ok @ (Just _) -> ok

capture' (RE_const _) (_, []) = Nothing
capture' (RE_const symbol) (revtoken, head_inp : input')
  | head_inp == symbol = Just (revtoken ++ [head_inp], input')
  | otherwise = Nothing

-- the OR of no clauses fails
capture' (RE_or []) _ = Nothing
capture' (RE_or (rexp : regexprs')) (revtoken, input) =
    case capture' rexp (revtoken, input) of
      Nothing -> capture' (RE_or regexprs') (revtoken, input)
      ok @ (Just _) -> ok

-- the AND of no clauses succeeds
capture' (RE_and []) (revtoken, input) = Just (revtoken, input)
capture' (RE_and (rexp : regexprs)) (revtoken, input) =
  case capture' rexp (revtoken, input) of
    Nothing -> Nothing
    ok @ (Just input') -> capture' (RE_and regexprs) input'
