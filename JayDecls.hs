-- The JayDecls module defines types and functions for interpreting
-- declarations in a Jay program.
--
-- The types and functions in this module were developed by
-- Alyce Brady, with input and ideas from the students in the
-- Principles of Programming Languages course in the Fall 2003
-- quarter:
--   Chris DiVirgilio, Zach Lynch, Stephen Robbins,
--   Eric Venner, and Becky Warner
--
-- November 2003

module JayDecls(DeclList, Declaration(Decl), JayType(JayInt, JayBool),
                initializeState,
                declarationTestSuite)
where

import TestSuiteSupportModule
import ValuesAndState

-----------------------------------------------
-----------------------------------------------
-- TYPES AND FUNCTIONS DEALING WITH DECLARATIONS
-----------------------------------------------
-----------------------------------------------

type DeclList = [ Declaration ]

data Declaration = Decl JayType [VariableName]

data JayType = JayInt | JayBool

-----------------------------------------------
-- initializeState declarationList
--    Generates the initial state of a Jay program from a declaration list.
--    The generated initial state contains all variables declared in the
--    declaration list, but without values; in other words, none of the
--    variables have been initialized.
-----------------------------------------------
initializeState :: DeclList -> State
initializeState decls = processDecls decls []

processDecls :: DeclList -> State -> State
processDecls [] state = state
processDecls (x:xs) state = processDecls xs (addDecl x state)

-----------------------------------------------
-- addDecl declaration state
--    Adds the variables declared in a declaration statement to the
--    given state; returns the new, modified state.
-----------------------------------------------
addDecl :: Declaration -> State -> State
addDecl (Decl jayType varNames) state = addVarsToState varNames state

addVarsToState :: [VariableName] -> State -> State
addVarsToState [] state = state
addVarsToState (x:xs) state = addVarsToState xs (addToState x state)

-------------------------------------------------
-------------------------------------------------
-- TEST CASES TO TEST THE JAY DECLARATIONS MODULE
-------------------------------------------------
-------------------------------------------------

-------------------------------------------------------------------
-- Functions that serve as abbreviations for common values
-- (see the ValuesAndState module for other abbreviations used here)
-------------------------------------------------------------------

testAdd3 = addToState "fib1" (addToState "fib0" (addToState "n" []))

testDecl1 = Decl JayInt ["n", "fib0", "fib1"]
testDecl2 = Decl JayInt ["temp", "result"]

testDeclList = [ testDecl1, testDecl2 ]

testDecl5Vars = initializeState testDeclList

-----------------------------------------------
-- declarationTestSuite
--    Evaluates to the tests for the declaration functions.
-----------------------------------------------
declarationTestSuite =
  TestSuite
  "Test the addDecl and initializeState functions."
  [
    -- Declare 0, 1, and more than 1 variables in empty state
        Test "Test an empty declaration (no variables) on an empty state"
             (addDecl (Decl JayInt []) [] == []),
        Test "Test a declaration of a single int variable on an empty state"
             (addDecl (Decl JayInt ["n"]) [] == [("n", UnInitialized)]),
        Test "Test a declaration of a single boolean variable"
             (addDecl (Decl JayBool ["eof"]) [] == [("eof", UnInitialized)]),
        Test "Test a declaration with multiple variables"
             (addDecl testDecl1 [] == testAdd3),

    -- Add empty declaration to non-empty state
        Test "Test an empty declaration (no variables) on a non-empty state"
             (addDecl (Decl JayInt []) testAdd3 == testAdd3),

    -- Test the creation of an initial state from declaration blocks
        Test "Test initializing the state with an empty declaration block"
             (initializeState [] == []),
        Test "Test initializing the state with a block of a single declaration"
             (initializeState [testDecl1] == testAdd3),
        Test "Test initializing state with a block of multiple declarations"
             (testDecl5Vars ==
             [("n", UnInitialized), ("fib0", UnInitialized),
              ("fib1", UnInitialized), ("temp", UnInitialized),
              ("result", UnInitialized)]),
    Test "A Placeholder Test" True
  ]

