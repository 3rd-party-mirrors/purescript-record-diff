module Record.Diff.Test where

import Prelude
import Data.List (List)
import Data.Traversable (traverse_)
import Data.Tuple (Tuple)
import Data.Variant (Variant, match)
import Effect (Effect)
import Effect.Console (log)
import ExpectInferred (expectInferred)
import Prim.RowList (Cons, Nil)
import Record.Diff (mismatches, rowListIntersection)
import Type.Prelude (Proxy(..), RLProxy)

testA :: Unit
testA =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))

    actual = rowListIntersection { a: 1, b: 2 } { a: 1, b: 2 }
  in
    expectInferred expected actual

testB :: Unit
testB =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))

    actual = rowListIntersection { a: 1, b: 2 } { a: 1, b: 2, c: "c" }
  in
    expectInferred expected actual

testC :: Unit
testC =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))

    actual = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2 }
  in
    expectInferred expected actual

testD :: Unit
testD =
  let
    expected = Proxy :: Proxy (RLProxy (Cons "a" Int (Cons "b" Int Nil)))

    actual = rowListIntersection { a: 1, b: 2, c: "c" } { a: 1, b: 2, d: "d" }
  in
    expectInferred expected actual

test1 ::
  List
    ( Variant
        ( a :: Tuple Int Int
        , b :: Tuple Int Int
        )
    )
test1 = mismatches { a: 1, b: 2 } { a: 2, b: 2 }

test2 ::
  List
    ( Variant
        ( a :: Tuple Int Int
        , b :: Tuple Int Int
        )
    )
test2 = mismatches { a: 1, b: 2 } { a: 2, b: 2, c: "c" }

test3 ::
  List
    ( Variant
        ( a :: Tuple Int Int
        , b :: Tuple Int Int
        )
    )
test3 = mismatches { a: 1, b: 2, c: "c" } { a: 1, b: 3 }

-- test4 ::
--   List
--     ( Variant
--         ( a :: Tuple Int Int
--         , b :: Tuple Int Int
--         )
--     )
test4 ::
  List
    ( Variant
        ( a :: Tuple Int Int
        , b :: Tuple Int Int
        )
    )
test4 = mismatches { a: 1, b: 2, c: "c" } { a: 2, b: 2, d: "d" }

main :: Effect Unit
main = do
  traverse_ log' test1
  traverse_ log' test2
  traverse_ log' test3
  traverse_ log' test4
  -- output:
  -- a was different: (Tuple 1 2)
  -- a was different: (Tuple 1 2)
  -- b was different: (Tuple 2 3)
  -- a was different: (Tuple 1 2)
  where
  log' =
    match
      { a: \x -> log $ "a was different: " <> show x
      , b: \x -> log $ "b was different: " <> show x
      }
