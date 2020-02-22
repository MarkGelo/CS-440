-- CS 440 -- Lexical scanner
-- Spring 2020, HW 5
--
-- The scanner matches regular expressions against some input and
-- returns a list of the captured tokens (and leftover input).

-- scan input produces Just (list of tokens, remaining input)
--
data Token = Const Int | Id String | Op String | Punct String | Space
    deriving (Show, Read, Eq)

-- scan input runs the scan algorithm on the input and returns the list of
-- resulting tokens (if any).  It calls a helper routine with an empty list
-- of result tokens.
--
scan :: String -> [Token]
scan input = case scan' scan_rexps [] input of
    Just(tokens,_) -> tokens
    Nothing -> []

-- scan' rexps revtokens input applies the first regular expression of
-- the list rexps to the input.  If the match succeeds, the captured
-- string is passed to a token-making routine associated with the reg
-- expr, and we add the new token to the head of our reversed list of
-- tokens found so far.  Exception: We don't add a Space token.
--
scan' _ revtokens []  = Just (reverse revtokens, [])
scan' (rexps:another) revtokens input = Nothing
-- *** STUB ***
{--
INPUT = "abc def 13 %"
scan' regexp [] input = recursively use scan_rexps
                      =  (head:scan_rexps) -> identifier=exp, Id= type      capture identifer input             [id abc] " def 13 %"
                      = (head: scan_rexps) -> operatore    ....                     operator input              [Id abc] " def 13 %"
                      =                                                                                         [Id abc, Id def] "13 %"
                      =                                                                                         [Id abc, Id def, Const 13]
                      =                                                                                         [Id abc, Id def, Const 13, Punct %]                

-}
-- scan_rexps are the regexprs to use to break up the input.  The format of each
-- pair is (regexpr, fcn); if capture regexpr produces a string str, apply the function
-- to it to get a token.  scan_rexps is an infinite cycle of the regular expressions
-- the scanner is looking for
--
scan_rexps = cycle
   [ -- *** STUB *** (num_const, ???), -- You need to write the tokenizer function
    (num_const, \x-> Const (read x)),
    (identifier, Id), (operators, Op), (punctuation, Punct), (spaces, \_ -> Space)]

num_const       = RE_and [digit1_9, RE_star digit]
digit1_9        = RE_in_set "123456789"
digit           = RE_in_set "0123456789"
operators       = RE_in_set "+-*/"
punctuation     = RE_in_set "[](){},;:.?!&#$%"

-- *** STUB *** need identifier, spaces
identifier      = RE_in_set "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
spaces          = RE_in_set " " -- HMMMM

-- Regular expressions
data RegExpr a
    = RE_const a
    | RE_or [RegExpr a]
    | RE_and [RegExpr a]
    | RE_star (RegExpr a)
    | RE_in_set [a]           -- [...] - any symbol in list
    | RE_empty                -- epsilon - empty (no symbols)
        deriving (Eq, Read, Show)

-- Regular expressions have their own types for token, reversed token,
-- and input.  (They're used to make type annotations more understandable.)
--
type RE_Token a = [a]
type RE_RevToken a = [a]
type RE_Input a = [a]

-- capture matches a regular expression against some input; on success,
-- it returns the matching token (= list of input symbols) and the
-- remaining input.  E.g. capture abcd abcdef = Just(abcd, ef)
--
capture  :: Eq a => RegExpr a -> RE_Input a -> Maybe(RE_Token a, RE_Input a)

-- top-level capture routine calls assistant with empty built-up token to start
--
capture rexp input = case capture' rexp ([], input) of
    Nothing -> Nothing
    Just (revtoken, input') -> Just (revtoken, input')


-- capture' rexp (partial_token input) matches the expression against
-- the input given a reversed partial token; on success, it returns
-- the completed token and remaining input. The token is in reverse
-- order. E.g., capture' cd (ba, cdef) = (dcba, ef)
--
capture' :: Eq a => RegExpr a -> (RE_RevToken a, RE_Input a) -> Maybe(RE_RevToken a, RE_Input a)

-- *** STUB *** add code for RE_const, RE_or, RE_and, RE_in_set, RE_star, RE_empty
-- *** and for any other kinds of regular expressions you need.
--

capture' RE_empty (revtoken, input)  = Just(revtoken, input)

capture' (RE_in_set []) (revtoken, input) = Nothing
capture' (RE_in_set symbols) (revtoken, head:input')
  | head `elem` symbols = case input' of
      [] -> Just (revtoken ++ [head], [])
      _ -> capture' (RE_in_set symbols) (revtoken ++ [head], input')
  | otherwise = Just (revtoken, head:input')

capture' (RE_star RE_empty) _ = Nothing
capture' (re_star @ (RE_star rexp)) input = Just input --capture' (RE_or [RE_and [rexp, re_star], RE_empty]) input
    {--
    case capture' (RE_or [RE_and [symbols, RE_star symbols], RE_empty]) (revtoken, input) of
        Nothing -> Nothing
        ok @ (Just _) -> ok
        -}
-- go look at the hw 04 shit, regxp* is (regxp regxp* | empty)
-- first regxp can be empty as well

capture' (RE_const _) (_, []) = Nothing
capture' (RE_const symbol) (revtoken, head_inp : input')
  | head_inp == symbol = Just (revtoken ++ [head_inp], input')
  | otherwise = Nothing

capture' (RE_or []) _ = Nothing
capture' (RE_or (rexp : regexprs')) (revtoken, input) =
    case capture' rexp (revtoken, input) of
      Nothing -> capture' (RE_or regexprs') (revtoken, input)
      ok @ (Just _) -> ok

capture' (RE_and []) (revtoken, input) = Just (revtoken, input)
capture' (RE_and (rexp : regexprs)) (revtoken, input) =
  case capture' rexp (revtoken, input) of
    Nothing -> Nothing
    ok @ (Just input') -> capture' (RE_and regexprs) input'
