import Data.Char
import Data.List
import Control.Monad (mfilter)

p1 = "{X=Y,Y=3}" --Y=3,X=3
p2 = "{X=1,X=3}" --fails tries to unify 1 and 3
p3 = "{f(a,Y)=f(X,b),c=Z}" --Z=c,Y=b,X=a
p4 = "{f(X)=g(Y)}" --fails different function names
p5 = "{f(X,Y)=f(X)}" --fails, different # of args'
p6 = "{f(f(f(f(a,Z),Y),X),W) = f(W,f(X,f(Y,f(Z,a))))}"

-- Language
--
--Problem ... {Equations}
-- ....
-- Expr   -> Term \+ Ttail
-- Ttail  -> \+ Term Ttail | empty
-- Term   -> Factor Ftail
-- Ftail  -> \* Factor Ftail | empty
-- Factor -> Id_or_Call | Paren | Negative | Constant
-- Paren  -> \( Expr \)
-- Negative -> \- Factor
--
-- Id_or_Call -> Id (Arguments | empty)
-- Arguments  -> \( Expr Argtail \)
-- Argtail    -> \, Expr Argtail | empty

-- The parse tree structure follows the grammar structure
--
type Equation = (Ptree, Ptree) -- expr = expr
type Problem = [Equation] -- so can have multiple equations
type Substitution = Equation -- Var -> ...

unify :: Problem -> Maybe (Problem, [Substitution])
unify problem = unify1 (problem, [])

unify1 :: (Problem, [Substitution]) -> Maybe (Problem, [Substitution])
unify1 problems = 
    do
    let 
        problemArr = fst problems -- get Problem
        sub = snd problems -- get Substitution
        --problems1 = checkEquality problemArr -- reduces if already ahve equality in equations
    if (Id "Const/Id", Id "Error") `elem` sub || (Id "Func", Id "Error") `elem` sub then Just (problemArr, sub)
    else (if length problemArr > 0 then unify2(problemArr, sub) else Just (problemArr, sub))

unify2 :: (Problem, [Substitution]) -> Maybe (Problem, [Substitution])
unify2 problems =
    do
        let
            p' = fst problems
            sub = snd problems
            equation = p' !! 0
            leftSide = fst equation
            rightSide = snd equation
        if isVar leftSide || isVar rightSide then
            (let 
                x = if isVar leftSide then leftSide else rightSide
                s = if x == leftSide then rightSide else leftSide
            in if isVar s && x == s then unify1 (drop 1 p', sub) else unify1 (substituteAll p' (x,s), (substituteAllSub sub (x,s)) ++ [(x, s)]))
            else 
                (if isId leftSide then (if leftSide == rightSide then unify1 (drop 1 p', sub) else unify1 (p', sub ++ [(Id "Const/Id", Id "Error")]))
                else (if isFunc leftSide && isFunc rightSide && getFuncN leftSide == getFuncN rightSide && length (getFuncA leftSide) == length (getFuncA rightSide)
                    then unify1 ((drop 1 p') ++ zip (getFuncA leftSide) (getFuncA rightSide), sub) else Just (p', sub ++ [(Id "Func", Id "Error")])))

--substitute function - takes in an equation and applies a substitution
--ex substitute (X = 3) (X -> 3)
--returns 3 = 3
substitute :: Equation -> Substitution -> Equation
substitute equation sub =
    do
        let
            subVar = fst sub
            subChange = snd sub
            eqLeft = fst equation
            eqRight = snd equation
            newLeft = if eqLeft == subVar then subChange else eqLeft
            newRight = if eqRight == subVar then subChange else eqRight -- need parse through functions otherwise wont substitute the var in side args
        (newLeft, newRight)

--substitutes all equations in the problem
substituteAll :: Problem -> Substitution -> Problem
substituteAll problem sub = map (\x -> substitute x sub) problem

substituteAllSub :: [Substitution] -> Substitution -> [Substitution]
substituteAllSub problem sub = map (\x -> substitute x sub) problem

unParse :: (Problem, [Char]) -> Maybe (Problem, [Char])
unParse ([], text) = Just ([], text)
unParse thing =
    do
        let
            problem = fst thing
            text = snd thing
        Just (problem, text)

getStrings :: Ptree -> [Char]
getStrings (Var out) = out
getStrings (Id out) = out
--getStrings (Call out tree) = map getStrings tree
getStrings _ = ""

-- for checking equality between equations
-- for example X = 3, Y = Y, 3 = 3
--returns X = 3
checkEquality :: Problem -> Problem
checkEquality = filter (\(leftSide, rightSide) -> leftSide /= rightSide)


-- cant do this shit unify1 (problemListmin1, sub ++ [(es1, exp1)])
--need to update orig problem array with updated substitution shit on all elements

--GO DO THE ALGORITHM WITH 2 EQUATIONS -- ONCE IT RETURNS A SUBSTITUTION, UPDATE THE EQUATIONS AND REDUCE IF POSSIBLE - THINK ONLY FOR FIRST PART
--WHAT IF IN THE SECOND PART WHERE IT ITERATES OVER THE ELEMENTS- SHOULD I UPDATE AECH TIME IT FINDS A SUBSTITION? I THINK SO
--BUT WHEN ONLY LEFT TO 1 EQUATION ,CALL ANOTHER METHOD THAT HANDLES THAT

--MAYBE WHEN FINDS A SUBSTITUTE, CALL THE SUBSTITUTE METHOD WHICH RETURNS THE SUBSTIUTION RESULT WHAT??????????

--WHEN IT FIND A SUBSTITUTION, SUBTITUTE IT TO THE PROBLEM AND THEN CALL A FUNCTION THAT CHECKS EACH EQUATION AND IF IT CAN BE TAKEN OUT, THE NCALL UNIFY ON THOSE PROBLEM

--AT FIRST CHECK EACH EQUATION FOR EQUALITY TO TAKE OUT ALREADY SOLVED EQUATIONS, THEN CHECK EACH EQUATION
--IF IT CAN BE SUBSTITUTED AND SOLVED ALREADY INDIVIDUALLY - LIKE IF X = 3, THAT CAN BE SOLVED ALREADY WITH X -> 3
--THEN AFTER CHECKS ALL THAT, USE UNIFY FUNCTION FOR 2 EQUATIONS AND SOLVE FROM THERE. HMMMMMMMMMMM
test :: (Ptree, Ptree) -> Bool
test _ = False

isVar :: Ptree -> Bool
isVar (Var _) = True
isVar _ = False

isId :: Ptree -> Bool
isId (Id _) = True
isId _ = False

isFunc :: Ptree -> Bool
isFunc (Call string _) = True
isFunc _ = False

--get function name
getFuncN :: Ptree -> [Char]
getFuncN (Call string _) = string

--get function args
getFuncA :: Ptree -> [Ptree]
getFuncA (Call string args) = args

parse :: String -> Problem
parse input = case parse_problem input of
    Just (parsing, "", eqs) -> eqs
    _ -> []

data Ptree =
    Empty                           -- The empty parse tree
    | Id String                     -- Identifiers
    | Var String                    -- variables that can be changed -- capitalized
    | Call String [Ptree]           -- Function call, list of arguments
    | Exp Ptree Ptree               -- For Term/Term_tail
    | Term Ptree Ptree              -- For Factor/Factor_Tail
    | Ttail Symbol Ptree Ptree      -- for Symbol Term Ttail
    | Ftail Symbol Ptree Ptree      -- Symbol Factor Ftail
    | Negative Ptree                -- For - factor
    | Equation Ptree Symbol Ptree   -- For expr = expr
    | Problem Ptree Ptree
    | Const Int                     --constant
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
lbracket = '{' :: Symbol
rbracket = '}' :: Symbol
equal = '=' :: Symbol

-- A generic parser returns Maybe a value and leftover input.
-- Most of the parsers are of type Parser Ptree.
--
type Parser t = Input -> Maybe (t, Input)

---------- PARSERS --------------------------------------------------
--Problem -> {Equations}
--Equations -> Equation EquationTail
--EquationTail -> \, Equations | Empty
--Equation -> Expr = Expr

type Parser1 t = Input -> Maybe(t,Input,Problem)

parse_problem :: Parser1 Ptree
parse_problem input =
    next_symbol lbracket input          `bind` (\ (_, input1) ->
    parse_equations input1              `bind` (\ (equations, input2,arr1) ->
    next_symbol rbracket input2         `bind` (\ (_, input3) ->
    Just (equations, input3,arr1))))

parse_equations :: Parser1 Ptree
parse_equations input = 
    parse_equation input            `bind` (\ (equation, input1, arr1) ->
    parse_equationTail input1       `bind` (\ (equationTail, input2,arr2) ->
    Just (Problem equation equationTail, input2, arr1 ++ arr2)))

parse_equationTail :: Parser1 Ptree
parse_equationTail input =
    next_symbol comma input             `bind` (\ (_, input1) ->
    parse_equations input1              `bind` (\ (equations, input2,arr2) ->
    Just (equations, input2,arr2)))
                                        `fails` (\() ->
    Just(Empty, dropSpaces input,[]))

parse_equation :: Parser1 Ptree
parse_equation input =
    parse_E input               `bind` (\ (expr, input1) ->
    next_symbol equal input1    `bind` (\ (equals, input2) ->
    parse_E input2              `bind` (\ (expr1, input3) ->
    Just (Equation expr equals expr1,input3,[(expr, expr1)]))))
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
-- Grammar rule: F -> id | ( E ) | Negative
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
    next_symbol star input      `bind` (\ (symbol, input1) ->
    parse_F input1              `bind` (\ (factor, input2) ->
    parse_Ftail input2          `bind` (\ (ftail, input3) ->
    Just (Ftail symbol factor ftail, input3) )))
                                `fails` (\() ->
    parse_empty input )


-- Identifiers and function calls both begin with an identifier,
-- but function calls follow with a parenthesized list of arguments.
--
-- Grammar rule: Id_or_Call -> Id (Arguments | empty)
--
parse_id_or_call :: Parser Ptree
parse_id_or_call input =
    getId (dropSpaces input)        `bind`  (\ (idstring, input1) ->
    parse_arguments input1          `bind`  (\ (argtail, input2) ->
    Just (Call idstring argtail, input2) )
                                    `fails` (\ () ->
    Just (Id idstring, input1) ))   `fails` (\ () ->
    getVar (dropSpaces input)       `bind` (\ (varString, input3) ->
    parse_arguments input3          `bind` (\ (argtail1, input4) ->
    Just (Call varString argtail1, input4))
                                    `fails` (\() ->
    Just (Var varString, input3))))


-- A set of function arguments is a parenthesized, comma-separated
-- list of argument expressions.  Unlike HW 4, here, there must be
-- at least one argument.  Note this routine returns a list of parse
-- trees, not one single parse tree.  (So it's of type Parser [Parser Ptree].)
--
-- Grammar rule: Arguments -> \( Expr Argtail \)
--
parse_arguments :: Parser [Ptree]
parse_arguments input =
    next_symbol lparen input    `bind`  (\ (_, input1) ->
    parse_E input1              `bind`  (\ (arg1, input2) ->
    parse_argtail input2        `bind`  (\ (args, input3) ->
    next_symbol rparen input3   `bind`  (\ (_, input4) ->
    Just (arg1:args, input4) ))))

--
-- An argument tail is a comma, argument expression, and argument tail, or
-- it's empty.  Note we return a list of parse trees, not just one.
--
-- Grammar rule: Argtail -> \, Expr Argtail | empty
--
parse_argtail :: Parser [Ptree]
parse_argtail input =
    next_symbol comma input     `bind`  (\(_, input1) ->
    parse_E input1              `bind`  (\(arg, input2) ->
    parse_argtail input2        `bind`  (\(args, input3) ->
    Just (arg:args, input3))))
                                `fails` (\() ->
    Just ([], input) )

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
    next_symbol minus input     `bind`  (\(_, input1) ->
    parse_F input1              `bind`  (\(factor, input2) ->
    Just (Negative factor, input2)))

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
    | (isLetter h && isLower h) || isDigit h =
        let (idtail, input2) = span (\c -> isAlphaNum c || c == '_') input1
        in Just (h:idtail, input2)
    | otherwise = Nothing

--similar to getId but getVar
getVar :: Parser String
getVar [] = Nothing
getVar (h:input1)
    | isLetter h && isUpper h =
        let (idtail, input2) = span (\c -> isAlphaNum c || c == '_') input1
        in Just (h:idtail, input2)
    | otherwise = Nothing

-- drop initial whitespace
--
dropSpaces x = dropWhile isSpace x