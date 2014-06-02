-- The JayFibSampleParseTree defines the makeSampleJayParseTree and
-- execSampleJayProgram functions.  The first function creates a
-- parse tree representing the sample Jay program on p. 43 of Tucker
-- and Noonan:
--
--     // compute result = the nth Fibonacci number
--     void main ()
--     {
--         int n, fib0, fib1;
--         int temp, result;
--         n = 8;
--         fib0 = 0;
--         fib1 = 1;
--         while (n > 0)
--         {
--             temp = fib0;
--             fib0 = fib1;
--             fib1 = fib0 + temp;
--             n = n - 1;
--         }
--         result = fib0;
--     }
--
-- The execSampleJayProgram executes the program above and returns
-- the final state of the program.
--
-- Author: Alyce Brady
--   based on the parse tree representation decided on by
--   Chris DiVirgilio, Zach Lynch, Stephen Robbins, Eric Venner,
--   and Becky Warner
--
-- November 10, 2003

module JayFibSampleParseTree where

import JayInterpreter

makeSampleJayParseTree =
  JayProgramParseTree "p. 43 subset"
    (Declarations
        [ Decl JayInt ["n", "fib0", "fib1"],
	  Decl JayInt ["temp", "result"]
	]
    )
    (ProgramBody
         [ Assign "n" (LiteralInt 8),
           Assign "fib0" (LiteralInt 0),
           Assign "fib1" (LiteralInt 1),
           While (GreaterThan (Variable "n") (LiteralInt 0))
                 (StmtBlock
                     [
                       Assign "temp" (Variable "fib0"),
                       Assign "fib0" (Variable "fib1"),
                       Assign "fib1" (Plus (Variable "fib0") (Variable "temp")),
                       Assign "n" (Minus (Variable "n") (LiteralInt 1))
                     ]
                 ),
            Assign "result" (Variable "fib0")
         ]
    )

execSampleJayProgram = execJayPgm makeSampleJayParseTree
