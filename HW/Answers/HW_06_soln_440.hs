-- CS 440 Spring 2020 Homework 6
-- (Skeleton with solution commented out)
--
-- Recursive descent parser using bind/fails.
-- Includes function calls f(x), f(x,x) [one or more arguments]
--

-- module <fill in> where
import Data.Char
import Data.List

-- Language
--
-- Expr   -> Term \+ Ttail
-- Ttail  -> \+ Term Ttail | empty
-- Term   -> Factor Ftail
-- Ftail  -> \* Factor Ftail | empty
-- Factor -> Id_or_Call | Paren | Negative
-- Paren  -> \( Expr \)
-- Negative -> \- Factor
--
-- Id_or_Call -> Id (Arguments | empty)
-- Arguments  -> \( Expr Argtail \)
-- Argtail    -> \, Expr Argtail | empty

-- The parse tree structure follows the grammar structure
--
data Ptree =
    Empty                           -- The empty parse tree
    | Id String                     -- Identifiers
    | Call String [Ptree]           -- Function call, list of arguments
    | Exp Ptree Ptree               -- For Term/Term_tail
    | Term Ptree Ptree              -- For Factor/Factor_Tail
    | Ttail Symbol Ptree Ptree      -- for Symbol Term Ttail
    | Ftail Symbol Ptree Ptree      -- Symbol Factor Ftail
    | Negative Ptree                -- For - factor
    deriving (Eq, Show, Read)

--
-- As before, symbols are just characters
--
type Symbol = Char
type Input  = [Symbol]

--
-- Some basic symbols
--
comma  = ',' :: Symbol 
lparen = '(' :: Symbol
minus  = '-' :: Symbol
plus   = '+' :: Symbol
rparen = ')' :: Symbol
star   = '*' :: Symbol 

-- A generic parser returns Maybe a value and leftover input.
-- Most of the parsers are of type Parser Ptree.
--
type Parser t = Input -> Maybe (t, Input)


---------- PARSERS --------------------------------------------------
--
-- For an expression, we look for a term and term_tail. If
-- the term tail is empty, we just return the underlying
-- term's parse tree.
--
-- Grammar rule: E -> T  Ttail
--
parse_E :: Parser Ptree
parse_E input =
    (parse_T input)         `bind` (\ (term, input1) ->
    (parse_Ttail input1)    `bind` (\ (ttail, input2) ->
    Just (make_tail Exp term ttail, input2) ))
        -- make_tail tries to build a short tree

--
-- A term is a factor and factor tail. If the term tail is
-- empty, we just return the underlying factor's parse tree
--
-- Grammar rule: T -> F Ftail
--
parse_T :: Parser Ptree
parse_T input =
    parse_F input       `bind`  (\ (factor, input1) ->
    parse_Ftail input1  `bind`  (\ (ftail, input2) ->
    Just (make_tail Term factor ftail, input2) ))
        -- make_tail tries to build a short tree

--
-- A term tail is a plus, term, and term tail, or it's empty.
--
-- Grammar rule : Ttail -> + T Ttail | empty
--
parse_Ttail :: Parser Ptree
parse_Ttail input =
    next_symbol plus input      `bind` (\ (symbol, input1) ->
    parse_T input1              `bind` (\ (term, input2) ->
    parse_Ttail input2          `bind` (\ (ttail, input3) ->
    Just (Ttail symbol term ttail, input3) )))
                                `fails` (\() ->
    parse_empty input )


-- A factor is an identifier or parenthesized expression or
-- a negative factor.
--
-- Grammar rule: F -> id | ( E )
--
parse_F :: Parser Ptree
parse_F input =
    parse_id_or_call input  `fails` (\() ->
    parse_paren_E input     `fails` (\() ->
    parse_negative input ))

--
-- A factor tail is a star, factor, and factor tail, or it's empty.
--
-- Grammar rule: Ftail -> \* F Ftail | empty
--
parse_Ftail :: Parser Ptree
parse_Ftail input =
    -- *** Stub - replace this with code ***

--    next_symbol star input      `bind` (\ (symbol, input1) ->
--    parse_F input1              `bind` (\ (factor, input2) ->
--    parse_Ftail input2          `bind` (\ (ftail, input3) ->
--    Just (Ftail symbol factor ftail, input3) )))
--                                `fails` (\() ->
--    parse_empty input )


-- Identifiers and function calls both begin with an identifier,
-- but function calls follow with a parenthesized list of arguments.
--
-- Grammar rule: Id_or_Call -> Id (Arguments | empty)
--
parse_id_or_call :: Parser Ptree
parse_id_or_call input =
    -- *** Stub - replace this with code ***

--    getId (dropSpaces input)        `bind`  (\ (idstring, input1) ->
--        parse_arguments input1      `bind`  (\ (argtail, input2) ->
--        Just (Call idstring argtail, input2) )
--                                    `fails` (\ () ->
--        Just (Id idstring, input1) ))


-- A set of function arguments is a parenthesized, comma-separated
-- list of argument expressions.  Unlike HW 4, here, there must be
-- at least one argument.  Note this routine returns a list of parse
-- trees, not one single parse tree.  (So it's of type Parser [Parser Ptree].)
--
-- Grammar rule: Arguments -> \( Expr Argtail \)
--
parse_arguments :: Parser [Ptree]
parse_arguments input =
    -- *** Stub - replace this with code ***

--    next_symbol lparen input    `bind`  (\ (_, input1) ->
--    parse_E input1              `bind`  (\ (arg1, input2) ->
--    parse_argtail input2        `bind`  (\ (args, input3) ->
--    next_symbol rparen input3   `bind`  (\ (_, input4) ->
--    Just (arg1:args, input4) ))))

--
-- An argument tail is a comma, argument expression, and argument tail, or
-- it's empty.  Note we return a list of parse trees, not just one.
--
-- Grammar rule: Argtail -> \, Expr Argtail | empty
--
parse_argtail :: Parser [Ptree]
parse_argtail input =
    -- *** Stub - replace this with code ***

--    next_symbol comma input     `bind`  (\(_, input1) ->
--    parse_E input1              `bind`  (\(arg, input2) ->
--    parse_argtail input2        `bind`  (\(args, input3) ->
--    Just (arg:args, input3))))
--                                `fails` (\() ->
--    Just ([], input) )

--
-- A parenthesized expression is surrounded by parentheses.
--
-- Grammar rule: Paren_E -> \( E \)
--
parse_paren_E :: Parser Ptree
parse_paren_E input =
    next_symbol lparen input    `bind` (\ (_, input1) ->
    parse_E input1              `bind` (\ (etree, input2) ->
    next_symbol rparen input2   `bind` (\ (_, input3) ->
    Just (etree, input3) )))

--
-- A negative factor is aunary minus followed by a factor.
--
-- Grammar rule: Negative -> \- Factor
--
parse_negative :: Parser Ptree
parse_negative input =
    -- *** Stub - replace this with code ***

--    next_symbol minus input     `bind`  (\(_, input1) ->
--    parse_F input1              `bind`  (\(factor, input2) ->
--    Just (Negative factor, input2)))

--
-- Parse empty parses no input and always succeeds.  This one
-- removes any leading spaces from the input before returning.
-- (You weren't required to do this.)
-- 
-- Grammar rule: epsilon
--
parse_empty input = Just(Empty, dropSpaces input)

--
-- next_symbol symbol input -- Check the input to see if its next
-- symbol matches the given one.  (Leading spaces are removed from the
-- input before the test.)  Note this parser returns a symbol, not a
-- parse tree.
--
next_symbol :: Symbol -> Parser Symbol
next_symbol symbol input =
    case dropSpaces input of
        [] -> Nothing
        (h:input1) | h == symbol -> Just(symbol, input1)
        _ -> Nothing

---------- UTILITIES --------------------------------------------------
--
-- The bind routine lets us take a Just val and run a function on the val.
-- If given Nothing instead, bind also yields Nothing.
--
bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing f = Nothing
bind (Just val) f = f val

--
-- The fails routine lets you call a function() if given Nothing; if
-- given Just val, the fails routine just yields that.
--
fails :: Maybe a -> (() -> Maybe a) -> Maybe a
fails Nothing f = f()
fails ok _ = ok

--
-- make_tail builds an expr using a term and term tail; it builds a term
-- using a factor and factor tail.  If the tail is empty, make_tail just
-- returns the given term or factor.  This optimization reduces the
-- number of skinny paths through the parse tree, which becomes shorter.
--
make_tail :: (Ptree -> Ptree -> Ptree) -> Ptree -> Ptree -> Ptree
make_tail _ ptree Empty = ptree
make_tail build ptree tailtree =
    build ptree tailtree

-- Remove initial whitespace and look for an identifier string.  If found,
-- return the string (and leftover input).
--
getId :: Parser String
getId [] = Nothing
getId (h:input1)
    | isLetter h =
        let (idtail, input2) = span (\c -> isAlphaNum c || c == '_') input1
        in Just (h:idtail, input2)
    | otherwise = Nothing

-- drop initial whitespace
--
dropSpaces x = dropWhile isSpace x