-- The JayStatements module defines types and functions for
-- executing statements in a Jay program.
--
-- The types and functions in this module were developed by
-- Alyce Brady, with input and ideas from the students in the
-- Principles of Programming Languages course in the Fall 2003
-- quarter:
--   Chris DiVirgilio, Zach Lynch, Stephen Robbins,
--   Eric Venner, and Becky Warner
--  
-- November 2003
    
module JayStatements
         (
            -- Imported From JayExpressions
                 Expr(LiteralInt, LiteralBool, Variable,
                      Multiplication, Division, Plus, Minus,
                      And, Or, EqualTo, NotEqualTo,
                      LessThan, LessThanEqualTo,
                      GreaterThan, GreaterThanEqualTo),
                 expressionTestSuite,
        
            -- Defined in JayStatements
                 Stmt(EmptyStmt, StmtBlock, Assign, If, While), Block,
                 execStmt,
                 statementTestSuite)
where

import TestSuiteSupportModule
import ValuesAndState
import JayDecls
import JayExpressions

-----------------------------------------------
-----------------------------------------------
-- TYPES AND FUNCTIONS DEALING WITH STATEMENTS
-----------------------------------------------
-----------------------------------------------

data Stmt = EmptyStmt |
            StmtBlock Block | 
            Assign VariableName Expr |
            If Expr Stmt Stmt |
            While Expr Stmt

type Block = [ Stmt ] 

-----------------------------------------------
-- execStmt statement state
--    Executes the specified program statement in the context of the
--    specified state; returns the new state of the program.
--    The function is defined in terms of each of the different
--    Stmt constructors.
-----------------------------------------------
execStmt :: Stmt -> State -> State
execStmt EmptyStmt state = state
execStmt (StmtBlock b) state = execBlock b state
execStmt (Assign v e) state = bind v (Assignable (evalExpr e state)) state
execStmt (If e s1 s2) state
	| (interpretAsBool (evalExpr e state)) = execStmt s1 state
	| otherwise = execStmt s2 state
execStmt (While e s) state
	| (interpretAsBool (evalExpr e state)) = execStmt (While e s) (execStmt s state)
	| otherwise = state

-----------------------------------------------
-- execBlock Block State
-- 	Provides the recursion to execute a block
--	(a list of statements). Called from
--	execStmt function.
-----------------------------------------------
execBlock :: Block -> State -> State
execBlock [] state = state
execBlock [s] state = execStmt s state
execBlock (s:b) state = execBlock b (execStmt s state)

------------------------------------------------
------------------------------------------------
-- TEST CASES TO TEST THE JAY STATEMENTS MODULE
------------------------------------------------
------------------------------------------------

-------------------------------------------------------------------
-- Functions that serve as abbreviations for common values
-- (see the ValuesAndState module for other abbreviations used here)
-------------------------------------------------------------------

value0 = Assignable (IntValue 0)
value1 = Assignable (IntValue 1)
value8 = Assignable (IntValue 8)

literal1 = LiteralInt 1

boolTrue = BoolValue True
boolFalse = BoolValue False

litTrue = LiteralBool True
litFalse = LiteralBool False

testDecl3 = initializeState [ Decl JayInt ["n", "fib0", "fib1"] ]
testDecl5 = initializeState [ Decl JayInt ["n", "fib0", "fib1"],
                              Decl JayInt ["temp", "result"] ]

testSet1Among5 = bind "n" value8 testDecl5
testSet3Among5 = bind "fib1" value1 (bind "fib0" value0 testSet1Among5)

testStmt1 = Assign "n" (LiteralInt 8)
testStmt2 = Assign "fib0" (LiteralInt 0)
testStmt3 = Assign "fib1" (LiteralInt 1)

testStmtBlock = [ testStmt1, testStmt2, testStmt3 ]

testDeclBool = initializeState [ Decl JayBool ["eof"] ]

testStmt4 = Assign "eof" litFalse

fib1 = (Variable "fib1")
testSub = Assign "fib1" (Minus fib1 literal1)

-----------------------------------------------
-- statementTestSuite
--    Evaluates to the tests for the statement functions.
-----------------------------------------------
statementTestSuite =
  TestSuite
  "Test the evalStmt function on the various Stmt constructors."
  [
    Test "Test empty statement in context of an empty state"
         (execStmt EmptyStmt [] == []),
    Test "Test empty statement in context of a non-empty state"
         (execStmt EmptyStmt testDecl5 == testDecl5),
    Test "Test a single assignment statement; state has multiple variables"
         (execStmt testStmt1 testDecl5 == testSet1Among5),
    Test "Test an empty statement block in context of a non-empty state"
         (execStmt (StmtBlock []) testDecl5 == testDecl5),
    Test "Test a statement block with a single statement (assignment)"
         (execStmt (StmtBlock [ testStmt1 ]) testDecl5 == testSet1Among5),
    Test "Test a statement block with a boolean assignment statement"
         (execStmt (StmtBlock [ testStmt4 ]) testDeclBool ==
         [("eof",(Assignable boolFalse))]),
    Test "Test a block of multiple statements"
         (execStmt (StmtBlock testStmtBlock) testDecl5 == testSet3Among5),
    Test "Test an if statement that goes to the then branch"
         (execStmt (If litTrue testStmt1 testStmt2) testDecl5 ==
          testSet1Among5),
    Test "Test an if statement that goes to the else branch"
         (execStmt (If litFalse testStmt1 testStmt2) testDecl3 ==
         [("n",UnInitialized), ("fib0",value0), ("fib1",UnInitialized)]),
    Test "Test a while statement that fails the first time"
         (execStmt (While litFalse testSub) testSet3Among5 == testSet3Among5),
    Test "Test a while statement that fails the second time"
         (execStmt (While (EqualTo fib1 literal1) testSub) testSet3Among5 ==
         [("n",value8), ("fib0",value0), ("fib1",value0),
          ("temp",UnInitialized), ("result", UnInitialized)]),
    Test "A Placeholder Test" True
  ]
