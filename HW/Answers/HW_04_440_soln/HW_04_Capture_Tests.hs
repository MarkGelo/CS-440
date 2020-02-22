-- CS 440 Spring 2020
-- Tests for capturing string matched by regular expression.

module HW_04_Capture_Tests where
import HW_04_Capture_soln

-- Lots of tests for capturing match of regular expressions

-- re uses explicit module name (just to show what it looks like)
--
re = HW_04_Capture_soln.RE_or [ RE_and [RE_const 'a', RE_const 'b'], RE_const 'c' ]

t01 = capture re "abcd" == Just ("ab", "cd") -- the ab in ab|c matched, leaving cd
t02 = capture re "cxyz" == Just ("c", "xyz") -- the c in ab|c matched, leaving xyz
t03 = capture re "qrst" == Nothing    -- both ab and c failed to match
t04 = capture re "acde" == Nothing    -- the a matched but b didn't match cde
                              -- and c didn't match acde

t05 = capture (RE_const "hi") ["hi", "there"] == Just (["hi"], ["there"])

or1 = RE_or [RE_const 1, RE_const 2]
t06 = capture or1 [1,2,3] == Just ([1], [2,3]) -- (1 or 2) matched the leading 1
t07 = capture or1 [2,3] == Just ([2], [3])     -- (1 or 2) matched the leading 2
t08 = capture or1 [3,2] == Nothing      -- (1 or 2) doesn't match leading 3

or2 = RE_or (map RE_const "abc")
t09 = capture or2 "axy" == Just ("a", "xy")
t10 = capture or2 "bcd" == Just ("b", "cd")
t11 = capture or2 "ccd" == Just ("c", "cd")
t12 = capture or2 "dba" == Nothing

or3 = RE_or (map RE_const ["hello","goodbye"])
t13 = capture or3 ["hello", "and", "goodbye"] == Just (["hello"], ["and", "goodbye"])
t14 = capture or3 ["goodbye", "and", "hello"] == Just (["goodbye"], ["and", "hello"])
t15 = capture or3 ["aloha"] == Nothing

abc = RE_and $ map RE_const "abc"   -- look for "a" then "b" then "c"
t16 = capture abc "abcd" == Just ("abc", "d")  -- "d" left after dropping "a", "b", "c"
t17 = capture abc "ab"   == Nothing   -- "a" and "b" ok but matching "c" fails

t18 = capture (RE_and [abc,abc]) "abcabcz" == Just ("abcabc", "z")
        -- match "abc", get Just (xxx, "abcz"), match "abc" and get Just (xxx, "z")
t19 = capture (RE_and [or3,or3]) ["hello","goodbye","okay?"] == Just (["hello","goodbye"], ["okay?"])
        -- 1st or2 matches "hello", 2nd matches "goodbye"
t20 = capture (RE_and [or3,or3]) ["hello","nope"] == Nothing
        -- 1st or3 matches "hello" but 2nd or3 doesn't match "nope"
