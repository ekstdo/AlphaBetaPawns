module Tests where

import Test.HUnit

test1 = TestCase (assertEqual "something: " (1,2) (1, 2))

tests = TestList [ TestLabel "test1" test1 ]
