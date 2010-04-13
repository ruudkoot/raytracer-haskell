module Tests.Data.Range where
import Prelude hiding (span)
import Test.QuickCheck
import Data.Range
import Data.List hiding (span)
import Control.Monad

newtype TRanges = TRanges { fromRanges::[(Int,Int)] } deriving (Show, Eq)
newtype TRange =  TRange { fromRange::(Int,Int) } deriving (Show,Eq)

tupleList::[a]->[(a,a)]
tupleList []       = []
tupleList [_]      = []
tupleList (x:y:ls) = (x,y):tupleList ls

instance Arbitrary TRanges where
  arbitrary = liftM  (TRanges .tupleList.sort.nub) (listOf arbitrary)

sort2::(Ord a)=>(a,a)->(a,a)
sort2 (x,y) = if x < y then (x,y) else (y,x)

instance Arbitrary TRange where
  arbitrary = liftM2 (curry $ TRange . sort2) arbitrary arbitrary

--Tests for the arbitrary instances
prop_rangesOK::TRanges -> Bool
prop_rangesOK = rangesOK.fromRanges

prop_rangeOK::TRange -> Bool
prop_rangeOK = rangeOK.fromRange

--Range validity for operations
prop_insertOK::TRange -> TRanges -> Bool
prop_insertOK (TRange r) (TRanges rs) = rangesOK $ insertRange r rs

prop_unionOK::TRanges -> TRanges -> Bool
prop_unionOK (TRanges r1) (TRanges r2) = rangesOK $ unionRanges r1 r2

prop_intersectOK::TRanges -> TRanges -> Bool
prop_intersectOK (TRanges r1) (TRanges r2) = rangesOK $ intersectRanges r1 r2

prop_diffOK::TRanges -> TRanges -> Bool
prop_diffOK (TRanges r1) (TRanges r2) = rangesOK $ diffRanges r1 r2

--Validity for operations using their span
prop_unionSpan::TRanges -> Property
prop_unionSpan (TRanges r) = not (null r) ==> unionRanges r [span r] == [span r]

prop_intersectSpan::TRanges -> Property
prop_intersectSpan (TRanges r) = not (null r) ==> intersectRanges r [span r] == r

prop_diffSpan1::TRanges -> Property
prop_diffSpan1 (TRanges r) = not (null r) ==> diffRanges r [span r] == []

prop_diffSpan2::TRanges -> Property
prop_diffSpan2 (TRanges r) = not (null r) ==> unionRanges r (diffRanges [span r] r) == [span r]

--Cross validity
{-
prop_unionComplement::TRanges -> Property
prop_unionComplement (TRanges r) = not (null r) ==> unionRanges r (complement r) == [span r]

prop_cValid::TRanges -> TRanges -> Property
prop_cValid (TRanges r1) (TRanges r2) = not (null r2) ==> diffRanges r1 r2 == intersectRanges r1 (complementIn (-10000,10000) r2)
-}
