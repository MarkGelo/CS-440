-- CS 440 Spring 2020
-- HW 6 - Parser & parse trees for function calls like f(e), f(e1,e2), etc.

module HW_06_Fcall_Tests where
import HW_06_Fcall


t01 = parse_E " xx  "
t02 = parse_E "  xy + yz"
t03 = parse_E "xy * yz"
t04 = parse_E "xy * yz + a + b*c  "
t05 = parse_E "-x "
t06 = parse_E " --x"
t07 = parse_E " - - x "
t08 = parse_E "ab+-cd"
t09 = parse_E "(  w )  "
t10 = parse_E "(x+y*z+-w)"
t11 = parse_E "((x))"
t12 = parse_E "f(x)"
t13 = parse_E "f(x,y)"
t14 = parse_E "f(x+y,z)"
t15 = parse_E "r + f(d, g(e, k*(ab+c)))"
t16 = parse_E "star(plus(a,b), c, plus(d, e))"
