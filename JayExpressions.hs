-- The JayExpressions module defines types and functions for
-- evaluating expressions in a Jay program.
--
-- The types and functions in this module were developed by
-- Alyce Brady, with input and ideas from the students in the
-- Principles of Programming Languages course in the Fall 2003
-- quarter:
--   Chris DiVirgilio, Zach Lynch, Stephen Robbins,
--   Eric Venner, and Becky Warner
--
-- November 2003

module JayExpressions (Expr(LiteralInt, LiteralBool, Variable,
                            Multiplication, Division, Plus, Minus,
                            And, Or, EqualTo, NotEqualTo,
                            LessThan, LessThanEqualTo,
                            GreaterThan, GreaterThanEqualTo),
                       evalExpr,
                       expressionTestSuite)
where

import TestSuiteSupportModule
import ValuesAndState

-----------------------------------------------
-----------------------------------------------
-- TYPES AND FUNCTIONS DEALING WITH EXPRESSIONS
-----------------------------------------------
-----------------------------------------------

data Expr = LiteralInt Integer |
            LiteralBool Bool |
            Variable VariableName |
            Multiplication Expr Expr |
            Division Expr Expr |
            Plus Expr Expr |
            Minus Expr Expr |
            And Expr Expr |
            Or Expr Expr |
            LessThan Expr Expr |
            LessThanEqualTo Expr Expr |
            GreaterThan Expr Expr |
            GreaterThanEqualTo Expr Expr |
            EqualTo Expr Expr |
            NotEqualTo Expr Expr
            deriving (Eq, Ord, Show)

type ArithOp = Integer -> Integer -> Integer

type LogicalOp = Bool -> Bool -> Bool

type BooleanOp = ValidValue -> ValidValue -> Bool

------------------------------------------------------
-- evalExpr expression state
--    Evaluates the specified expression in the context of the
--    specified state; returns the value of the expression.
--    The function is defined in terms of each of the different
--    Expr constructors.
------------------------------------------------------
evalExpr :: Expr -> State -> ValidValue

-- Constants and Variables
evalExpr (LiteralInt value) state = (IntValue value)
evalExpr (LiteralBool value) state = (BoolValue value)
evalExpr (Variable var) state = deref var state 

-- Arithmetic Operations
evalExpr (Multiplication e1 e2) state = evalArithOp (*) e1 e2 state
evalExpr (Division e1 e2) state = evalArithOp (div) e1 e2 state
evalExpr (Plus e1 e2) state = evalArithOp (+) e1 e2 state
evalExpr (Minus e1 e2) state = evalArithOp (-) e1 e2 state

-- Logical Operations
evalExpr (And e1 e2) state = evalLogicalOp (&&) e1 e2 state
evalExpr (Or e1 e2) state = evalLogicalOp (||) e1 e2 state

-- Relational Operations
evalExpr (LessThan e1 e2) state = evalBoolOp (<) e1 e2 state
evalExpr (LessThanEqualTo e1 e2) state = evalBoolOp (<=) e1 e2 state
evalExpr (GreaterThan e1 e2) state = evalBoolOp (>) e1 e2 state
evalExpr (GreaterThanEqualTo e1 e2) state = evalBoolOp (>=) e1 e2 state 

-- Equality-Testing Relational Operations
evalExpr (EqualTo e1 e2) state = evalBoolOp (==) e1 e2 state
evalExpr (NotEqualTo e1 e2) state = evalBoolOp (/=) e1 e2 state

------------------------------------------------------
-- Helper methods used by various evalExpr definitions
------------------------------------------------------

------------------------------------------------------
-- evalArithOp arithmeticOperator e1 e2 state
--    Applies the specified binary arithmetic operator to the two
--    integer expressions e1 and e2 in the context of the given state;
--    returns the appropriate integer value
------------------------------------------------------
evalArithOp :: ArithOp -> Expr -> Expr -> State -> ValidValue
evalArithOp op e1 e2 state =
    IntValue (op (interpretAsInt (evalExpr e1 state))
                 (interpretAsInt (evalExpr e2 state)))

------------------------------------------------------
-- evalLogicalOp logicalOperator e1 e2 state
--    Applies the specified binary logic operator to the two
--    boolean expressions e1 and e2 in the context of the given state;
--    returns the appropriate boolean value
------------------------------------------------------
evalLogicalOp :: LogicalOp -> Expr -> Expr -> State -> ValidValue
evalLogicalOp op e1 e2 state =
    BoolValue (op (interpretAsBool (evalExpr e1 state))
                  (interpretAsBool (evalExpr e2 state)))

------------------------------------------------------
-- evalBoolOp booleanOperator e1 e2 state
--    Applies the specified binary boolean operator to the two
--    expressions e1 and e2 in the context of the given state;
--    returns the appropriate boolean value
------------------------------------------------------
evalBoolOp :: BooleanOp -> Expr -> Expr -> State -> ValidValue
evalBoolOp op e1 e2 state =
    BoolValue (op (evalExpr e1 state) (evalExpr e2 state))



------------------------------------------------
------------------------------------------------
-- TEST CASES TO TEST THE JAY EXPRESSIONS MODULE
------------------------------------------------
------------------------------------------------

-------------------------------------------------------------------
-- Functions that serve as abbreviations for common values
-- (see the ValuesAndState module for other abbreviations used here)
-------------------------------------------------------------------

int0 = IntValue 0
int1 = IntValue 1
int3 = IntValue 3
int5 = IntValue 5
int8 = IntValue 8
int15 = IntValue 15
int16 = IntValue 16

boolTrue = BoolValue True
boolFalse = BoolValue False

value0 = Assignable int0
value1 = Assignable int1
value8 = Assignable int8

literal3 = LiteralInt 3
literal5 = LiteralInt 5
literal8 = LiteralInt 8
literal16 = LiteralInt 16

litTrue = LiteralBool True
litFalse = LiteralBool False

testAdd3 = addToState "fib1" (addToState "fib0" (addToState "n" []))
testSet3 = bind "fib1" value1 (bind "fib0" value0 (bind "n" value8 testAdd3))

testSetBool = bind "eof" (Assignable boolFalse) (addToState "eof" [])

------------------------------------------------------
-- expressionTestSuite
--    Evaluates to the tests for the expression functions.
------------------------------------------------------
expressionTestSuite =
  TestSuite
  "Test the evalExpr function on the various Expr constructors."
  [
    Test "Test integer literal evaluation"
         (evalExpr literal8 [] == int8),
    Test "Test boolean literal evaluation"
         (evalExpr litFalse [] == boolFalse),
    Test "Test evaluating the first variable in the state"
         (evalExpr (Variable "n") testSet3 == int8),
    Test "Test evaluating the last variable in the state"
         (evalExpr (Variable "fib1") testSet3 == int1),
    Test "Test evaluating a boolean variable"
         (evalExpr (Variable "eof") testSetBool == boolFalse),
    Test "Test an arithmetic binary operator (Plus)"
         (evalExpr (Plus literal3 literal5) [] == int8),
    Test "Test another arithmetic binary operator (Multiplication)"
         (evalExpr (Multiplication literal3 literal5) [] == int15),
    Test "Test the division operator"
         (evalExpr (Division literal16 literal5) [] == int3),
    Test "Test a logical binary operator (And)"
         (evalExpr (And litTrue litTrue) [] == boolTrue),
    Test "Test another logical binary operator (Or)"
         (evalExpr (Or (Variable "eof") litTrue) testSetBool == boolTrue),
    Test "Test a relational binary operator (LessThan)"
         (evalExpr (LessThan literal3 literal5) [] == boolTrue),
    Test "Test another relational binary operator (GreaterThanEqualTo)"
         (evalExpr (GreaterThanEqualTo literal3 literal5) [] == boolFalse),
    Test "Test an equality test on integers"
         (evalExpr (EqualTo literal3 literal5) [] == boolFalse),
    Test "Test an equality test on boolean values"
         (evalExpr (EqualTo litTrue litTrue) [] == boolTrue),
    Test "A Placeholder Test" True
  ]
