module Record.Diff where

import Prelude
import Data.List (List)
import Data.Tuple (Tuple(..))
import Data.Variant (Variant, expand, inj)
import Prim.Row as Row
import Prim.RowList (kind RowList, Cons, Nil)
import Prim.Symbol as Symbol
import Record as Record
import Type.Data.Boolean (class If)
import Type.Data.Ordering (class Equals)
import Type.Prelude (class IsSymbol, class RowToList, LT, RLProxy(..), SProxy(..))

class RowListIntersection (xs :: RowList) (ys :: RowList) (res :: RowList) | xs ys -> res

instance rliNilXS :: RowListIntersection Nil (Cons name ty tail) Nil
else instance rliNilYS :: RowListIntersection (Cons name ty tail) Nil Nil
else instance rliNilNil :: RowListIntersection Nil Nil Nil
else instance rliMatch ::
  ( RowListIntersection xTail yTail tail
    ) =>
  RowListIntersection (Cons name ty xTail) (Cons name ty yTail) (Cons name ty tail)
else instance rliConsCons ::
  ( Symbol.Compare xname yname ord
  , Equals ord LT isLt
  , If
      isLt
      (RLProxy xs)
      (RLProxy (Cons xname xty xs))
      (RLProxy xs')
  , If
      isLt
      (RLProxy (Cons yname yty ys))
      (RLProxy ys)
      (RLProxy ys')
  , RowListIntersection xs' ys' res
  ) =>
  RowListIntersection (Cons xname xty xs) (Cons yname yty ys) res

rowListIntersection ::
  ∀ x xs y ys zs.
  RowToList x xs =>
  RowToList y ys =>
  RowListIntersection xs ys zs =>
  { | x } ->
  { | y } ->
  RLProxy zs
rowListIntersection _ _ = RLProxy

class RecordDiff (rl :: RowList) (r1 :: # Type) (r2 :: # Type) (tuples :: # Type) | rl -> r1 r2 tuples where
  recordDiff :: RLProxy rl -> { | r1 } -> { | r2 } -> List (Variant tuples)

instance rdNil :: RecordDiff Nil trash1 trash2 () where
  recordDiff _ _ _ = mempty

instance rdCons ::
  ( IsSymbol name
  , Eq ty
  , Row.Cons name ty trash1 r1
  , Row.Cons name ty trash2 r2
  , Row.Cons name (Tuple ty ty) tuples' tuples
  , Row.Union tuples' trash tuples
  , RecordDiff tail r1 r2 tuples'
  ) =>
  RecordDiff
    (Cons name ty tail)
    r1
    r2
    tuples where
  recordDiff _ r1 r2 = first <> rest
    where
    namep = SProxy :: SProxy name

    first
      | l <- Record.get namep r1
      , r <- Record.get namep r2
      , l /= r = pure (inj namep (Tuple l r))
      | otherwise = mempty

    rest = expand <$> recordDiff (RLProxy :: RLProxy tail) r1 r2

mismatches ::
  ∀ r1 rl1 r2 rl2 rl tuples.
  RowToList r1 rl1 =>
  RowToList r2 rl2 =>
  RowListIntersection rl1 rl2 rl =>
  RecordDiff rl r1 r2 tuples =>
  { | r1 } ->
  { | r2 } ->
  List (Variant tuples)
mismatches r1 r2 = recordDiff (RLProxy :: RLProxy rl) r1 r2
