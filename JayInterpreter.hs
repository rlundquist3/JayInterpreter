-- The JayInterpreter module defines the execJayPgm function,
-- which executes a Jay program represented as a parse tree.
-- It supports all of the features in the initial specification
-- of the Jay language.
--
-- A Jay parse tree is of the form:
--
--   JayProgramParseTree "program description, e.g., nonsense program"
--     (Declarations
--         [ Decl JayInt ["v1", "v2", "v3"],
--           Decl JayBool ["v4", "v5"]
--         ]
--     )
--     (ProgramBody
--          [ Assign "v1" (LiteralInt 1),
--            Assign "v2" (LiteralInt 2),
--            Assign "v4" (LiteralBool False),
--            Assign "v5" (Variable "v4"),
--            If (LessThan (Variable "v1") (Variable "v2"))
--               -- Then Branch
--                  (StmtBlock
--                      [
--                         Assign "v1" (Plus (Variable "v1") (Variable "v2")),
--                         Assign "v5" (And (Variable "v4") (Variable "v5")),
--                      ]
--                  )
--               -- Else Branch
--                  EmptyStmt,
--            While (GreaterThan (Variable "v2") (LiteralInt 0))
--                  Assign "v2" (Minus (Variable "v2") (Variable "v1"))
--          ]
--     )
--
-- The valid variable types are JayInt and JayBool.  Jay constant
-- expressions are formed by applying the LiteralInt and LiteralBool
-- expression constructor functions to integer or boolean constants.
-- Variable expressions are formed by applying the Variable expression
-- constructor function to a variable name.  Arithmetic and logical
-- expressions are formed by applying the following expression
-- constructor functions:
--    Multiplication intExpression intExpression
--    Division intExpression intExpression
--    Plus intExpression intExpression
--    Minus intExpression intExpression
--    And boolExpression boolExpression
--    Or boolExpression boolExpression
--    EqualTo intOrBoolExpression intOrBoolExpression
--    NotEqualTo intOrBoolExpression intOrBoolExpression
--    LessThan intExpression intExpression
--    LessThanEqualTo intExpression intExpression
--    GreaterThan intExpression intExpression
--    GreaterThanEqualTo intExpression intExpression
-- Valid statements include the empty statement, assignment,
-- conditional, and loop statements illustrated in the example above
-- and statement blocks (a list of statements preceded by the
-- StmtBlock constructor), also illustrated in the example.
--
-- The execJayPgm function does no type-checking, assuming that
-- that has been done as part of the compilation process that
-- produced the parse tree.  Nor does it check for other errors
-- that would be caught by the compilation process, such as
-- dereferencing undeclared or uninitialized variables.
--
-- The Jay semantics interpreter in this module was developed by
-- Alyce Brady, with input and ideas from the students in the
-- Principles of Programming Languages course in the Fall 2003
-- quarter:
--   Chris DiVirgilio, Zach Lynch, Stephen Robbins,
--   Eric Venner, and Becky Warner
--
-- November 2003

module JayInterpreter
         (
            -- Imported From ValuesAndState
                 stateFunctionsTestSuite,

            -- Imported From JayDecls
                 Declaration(Decl), JayType(JayInt, JayBool),
                 declarationTestSuite,

            -- Imported From JayExpressions
                 Expr(LiteralInt, LiteralBool, Variable,
                      Multiplication, Division, Plus, Minus,
                      And, Or, EqualTo, NotEqualTo,
                      LessThan, LessThanEqualTo,
                      GreaterThan, GreaterThanEqualTo),
                 expressionTestSuite,

            -- Imported From JayStatements
                 Stmt(EmptyStmt, StmtBlock, Assign, If, While),
                 statementTestSuite,

            -- Defined in JayInterpreter
                 JayProgramAsParseTree(JayProgramParseTree),
                 DeclarationList(Declarations), Body(ProgramBody),
                 execJayPgm,
                 programTestSuite, allJayInterpreterTests)
where

import TestSuiteSupportModule
import ValuesAndState
import JayDecls
import JayStatements       -- which imports JayExpressions

-----------------------------------------------
-----------------------------------------------
-- TYPES AND FUNCTIONS DEALING WITH PROGRAMS
-----------------------------------------------
-----------------------------------------------

data JayProgramAsParseTree = JayProgramParseTree String DeclarationList Body

data DeclarationList = Declarations DeclList

data Body = ProgramBody Block

---------------------------------------------------------------------
-- execJayPgm program
--    Executes a Jay Program; returns the final state of the program.
---------------------------------------------------------------------
execJayPgm :: JayProgramAsParseTree -> State
execJayPgm (JayProgramParseTree name decl body) = execStmt (StmtBlock (interpretAsBlock body)) (initializeState (interpretAsDeclList decl))

interpretAsBlock :: Body -> Block
interpretAsBlock (ProgramBody b) = b

interpretAsDeclList :: DeclarationList -> DeclList
interpretAsDeclList (Declarations d) = d

------------------------------------------------
------------------------------------------------
-- TEST CASES TO TEST THE JAY STATEMENTS MODULE
------------------------------------------------
------------------------------------------------

----------------------------------------------------------
-- Functions that serve as abbreviations for common values
-- (see the JayDecls and JayStatements modules for other
-- abbreviations used here)
----------------------------------------------------------

value0 = Assignable (IntValue 0)
value1 = Assignable (IntValue 1)
value8 = Assignable (IntValue 8)

testPgm = 
  JayProgramParseTree "testProgram"
    (Declarations
        [ Decl JayInt ["n", "fib0", "fib1"],
          Decl JayInt ["temp", "result"]
        ]
    )
    (ProgramBody
         [ Assign "n" (LiteralInt 8),
           Assign "fib0" (LiteralInt 0),
           Assign "fib1" (LiteralInt 1)
         ]
    )

-------------------------------------------------------
-- programTestSuite
--    Evaluates to the tests for the program functions.
-------------------------------------------------------
programTestSuite =
  TestSuite
  "Test the execJayPgm function."
  [
    Test "Test program with 5 declared variables and 3 assignments"
         (execJayPgm testPgm ==
             [("n",value8), ("fib0",value0), ("fib1",value1),
              ("temp", UnInitialized), ("result", UnInitialized)]),
    Test "A Placeholder Test" True
  ]

---------------------------------------------------------------------
-- allTests
--    Evaluates to the complete set of tests for the Jay Interpreter.
---------------------------------------------------------------------
allJayInterpreterTests =
  TestSuite
  "Test all the components of the Jay Interpreter program."
  [ 
     stateFunctionsTestSuite,
     declarationTestSuite,
     expressionTestSuite,
     statementTestSuite,
     programTestSuite,
     Test "A Placeholder Test" True
 ]
