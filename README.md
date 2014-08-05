JayInterpreter
==============

Riley Lundquist

May 2014

This is an interpreter for the simple Jay language written in Haskell.

Running the Program:
--------------------
You must have GHC or hugs installed to run JayInterpreter.

```
$ ghci JayInterpreter.hs
```
Should result in output similar to:
```
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
[1 of 6] Compiling TestSuiteSupportModule ( TestSuiteSupportModule.hs, interpreted )
[2 of 6] Compiling ValuesAndState   ( ValuesAndState.hs, interpreted )
[3 of 6] Compiling JayDecls         ( JayDecls.hs, interpreted )
[4 of 6] Compiling JayExpressions   ( JayExpressions.hs, interpreted )
[5 of 6] Compiling JayStatements    ( JayStatements.hs, interpreted )
[6 of 6] Compiling JayInterpreter   ( JayInterpreter.hs, interpreted )
Ok, modules loaded: JayInterpreter, TestSuiteSupportModule, ValuesAndState, JayDecls, JayStatements, JayExpressions.
```
To run the test suite showing all test cases:
```
*JayInterpreter> allJayInterpreterTests
Test all the components of the Jay Interpreter program.
  Test the addToState, bind, and deref functions.
    Test adding a single variable to an empty state: Passed
    Test adding multiple variables to the state: Passed
    Test binding to first variable in the state: Passed
    Test binding to first, middle, and last variables in the state: Passed
    Test modifying first variable in the state: Passed
    Test modifying last variable in the state: Passed
    Test dereferencing the first variable in the state: Passed
    Test dereferencing the middle variable in the state: Passed
    Test dereferencing the last variable in the state: Passed
    Test dereferencing the only initialized variable in the state: Passed
    Test binding to a boolean variable: Passed
    Test dereferencing a boolean variable: Passed
    A Placeholder Test: Passed
  Test the addDecl and initializeState functions.
    Test an empty declaration (no variables) on an empty state: Passed
    Test a declaration of a single int variable on an empty state: Passed
    Test a declaration of a single boolean variable: Passed
    Test a declaration with multiple variables: Passed
    Test an empty declaration (no variables) on a non-empty state: Passed
    Test initializing the state with an empty declaration block: Passed
    Test initializing the state with a block of a single declaration: Passed
    Test initializing state with a block of multiple declarations: Passed
    A Placeholder Test: Passed
  Test the evalExpr function on the various Expr constructors.
    Test integer literal evaluation: Passed
    Test boolean literal evaluation: Passed
    Test evaluating the first variable in the state: Passed
    Test evaluating the last variable in the state: Passed
    Test evaluating a boolean variable: Passed
    Test an arithmetic binary operator (Plus): Passed
    Test another arithmetic binary operator (Multiplication): Passed
    Test the division operator: Passed
    Test a logical binary operator (And): Passed
    Test another logical binary operator (Or): Passed
    Test a relational binary operator (LessThan): Passed
    Test another relational binary operator (GreaterThanEqualTo): Passed
    Test an equality test on integers: Passed
    Test an equality test on boolean values: Passed
    A Placeholder Test: Passed
  Test the evalStmt function on the various Stmt constructors.
    Test empty statement in context of an empty state: Passed
    Test empty statement in context of a non-empty state: Passed
    Test a single assignment statement; state has multiple variables: Passed
    Test an empty statement block in context of a non-empty state: Passed
    Test a statement block with a single statement (assignment): Passed
    Test a statement block with a boolean assignment statement: Passed
    Test a block of multiple statements: Passed
    Test an if statement that goes to the then branch: Passed
    Test an if statement that goes to the else branch: Passed
    Test a while statement that fails the first time: Passed
    Test a while statement that fails the second time: Passed
    A Placeholder Test: Passed
  Test the execJayPgm function.
    Test program with 5 declared variables and 3 assignments: Passed
    A Placeholder Test: Passed
  A Placeholder Test: Passed
```
