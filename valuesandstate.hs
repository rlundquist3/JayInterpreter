-- The ValuesAndState module defines types and functions for binding
-- valid values to variables in a Jay program and dereferencing them.
-- with valid values and variables in a Jay program (int and boolean)
-- The binding and dereferencing functions do no type-checking,
-- assuming that that has been done as part of the compilation
-- process that produced the parse tree.
--
-- The types and functions in this module were developed by
-- Alyce Brady, with input and ideas from the students in the
-- Principles of Programming Languages course in the Fall 2003
-- quarter:
--   Chris DiVirgilio, Zach Lynch, Stephen Robbins,
--   Eric Venner, and Becky Warner
--
-- November 2003

module ValuesAndState(ValidValue(IntValue, BoolValue),
                      AssignableValue(UnInitialized, Assignable),
                      VariableName, State,
                      interpretAsInt, interpretAsBool,
                      addToState, bind, deref,
                      stateFunctionsTestSuite)
where

import TestSuiteSupportModule
import Data.List (takeWhile, dropWhile)

--------------------------------------------------------------------
--------------------------------------------------------------------
-- TYPES AND FUNCTIONS DEALING WITH VALID VALUES, VARIABLE BINDINGS,
-- AND ACCESSING THE PROGRAM STATE
--------------------------------------------------------------------
--------------------------------------------------------------------

data ValidValue = IntValue Integer | BoolValue Bool
		  deriving (Eq, Ord, Show)

data AssignableValue = UnInitialized | Assignable ValidValue

type VariableName = String

type VarBinding = (VariableName, AssignableValue)

type State = [VarBinding]

instance Show AssignableValue where
	show UnInitialized = "**unitialized**"
	show (Assignable v) = show v

instance Eq AssignableValue where
	UnInitialized == UnInitialized = True
	(Assignable v1) == (Assignable v2) = v1 == v2
	v1 == v2 = False

-------------------------------------------------------------------
-- Functions that interpret a ValidValue as an Integer or Bool,
-- providing the reverse of the IntValue and BoolValue constructors.
-------------------------------------------------------------------

-------------------------------------------------------------------
-- interpretAsInt value
--    Returns the integer associated with the specified value;
--    generates a run-time error if the value is not an integer.
-------------------------------------------------------------------
interpretAsInt :: ValidValue -> Integer
interpretAsInt (IntValue v) = v

-------------------------------------------------------------------
-- interpretAsBool value
--    Returns the boolean value associated with the specified value;
--    generates a run-time error if the value is not a boolean value.
-------------------------------------------------------------------
interpretAsBool :: ValidValue -> Bool
interpretAsBool (BoolValue v) = v

-------------------------------------------------------------------
-- interpretAsValidValue value
--	Returns the ValidValue associated with the AssignableValue;
--	generates a run-time error if the value is not a ValidValue
--	that is, if it is UnInitialized.
-------------------------------------------------------------------
interpretAsValidValue :: AssignableValue -> ValidValue
interpretAsValidValue (Assignable (IntValue v)) = IntValue v
interpretAsValidValue (Assignable (BoolValue v)) = BoolValue v

-------------------------------------------------------------------
-- addToState var state
--    Adds the named variable to the state as an uninitialized variable.
--    The parsing phase ensures that no variables are redeclared.
-------------------------------------------------------------------
addToState :: VariableName -> State -> State
addToState n s = s ++ [(n, UnInitialized)]

-------------------------------------------------------------------
-- bind var value state
--    Binds the named variable to the specified value in the state.
--    The parsing phase ensures that no undeclared variables are bound
--    to a value.
-------------------------------------------------------------------
bind :: VariableName -> AssignableValue -> State -> State
bind n v s = takeWhile ((/=n).fst) s ++ (n, v):tail (dropWhile ((/=n).fst) s)

-------------------------------------------------------------------
-- deref var state
--    Retrieves (dereferences) the value of the named variable
--    from the state.
--    The parsing phase ensures that no undeclared variables are
--    dereferenced, and also that programs never dereference an
--    uninitialized variable, although that's a harder problem.
-------------------------------------------------------------------
deref :: VariableName -> State -> ValidValue
deref n s = interpretAsValidValue (snd ([b | b <- s, fst b == n] !! 0))

-------------------------------------------------
-------------------------------------------------
-- TEST CASES TO TEST THE VALUES AND STATE MODULE
-------------------------------------------------
-------------------------------------------------

-------------------------------------------------------------------
-- Functions that serve as abbreviations for common values
-------------------------------------------------------------------

int0 = IntValue 0
int1 = IntValue 1
int8 = IntValue 8

value0 = Assignable (IntValue 0)
value1 = Assignable (IntValue 1)
value8 = Assignable (IntValue 8)
value16 = Assignable (IntValue 16)

testAdd3 = addToState "fib1" (addToState "fib0" (addToState "n" []))
testSet3 = bind "fib1" value1 (bind "fib0" value0 (bind "n" value8 testAdd3))
testAdd3Set1 = bind "fib0" value0 testAdd3

testSetBool = bind "eof" (Assignable (BoolValue False)) (addToState "eof" [])

-----------------------------------------------------
-- stateTestSuite
--    Evaluates to the tests for the state functions.
-----------------------------------------------------
stateFunctionsTestSuite =
  TestSuite
  "Test the addToState, bind, and deref functions."
  [
    -- Add 1 and more than 1 variables to a state
        Test "Test adding a single variable to an empty state"
             (addToState "n" [] == [("n",UnInitialized)]),
        Test "Test adding multiple variables to the state"
             (testAdd3 ==
             [("n", UnInitialized), ("fib0", UnInitialized),
              ("fib1", UnInitialized)]),

    -- Bind integer variables to beginning and end of state
        Test "Test binding to first variable in the state"
             (bind "n" value8 testAdd3 ==
             [("n", value8), ("fib0", UnInitialized), ("fib1", UnInitialized)]),
        Test "Test binding to first, middle, and last variables in the state"
             (testSet3 ==
             [("n",value8), ("fib0",value0), ("fib1",value1)]),
        Test "Test modifying first variable in the state"
             (bind "n" value16 testSet3 ==
             [("n",value16), ("fib0",value0), ("fib1",value1)]),
        Test "Test modifying last variable in the state"
             (bind "fib1" value16 testSet3 ==
             [("n",value8), ("fib0",value0), ("fib1",value16)]),

    -- Dereference integer variables at beginning/ end of state
        Test "Test dereferencing the first variable in the state"
             (deref "n" testSet3 == int8),
        Test "Test dereferencing the middle variable in the state"
             (deref "fib0" testSet3 == int0),
        Test "Test dereferencing the last variable in the state"
             (deref "fib1" testSet3 == int1),
        Test "Test dereferencing the only initialized variable in the state"
             (deref "fib0" testAdd3Set1 == int0),

    -- Bind and dereference boolean variables
        Test "Test binding to a boolean variable"
             (testSetBool == [("eof",(Assignable (BoolValue False)))]),
        Test "Test dereferencing a boolean variable"
             (deref "eof" testSetBool == (BoolValue False)),
    Test "A Placeholder Test" True
	]

