module Data.Range where
import Data.Set


type Range a = (a,a)
type Ranges a = [Range a]

--Invariants
rangeOK::(Ord a) => Range a -> Bool
rangeOK (l,h) = l < h

rangesOK::(Ord a) => Ranges a -> Bool
rangesOK []     = True
rangesOK (_:[]) = True
rangesOK (r1:r2:xs) = rangeOK r1 && rangeOK r2 && disjoint r1 r2 && rangesOK (r2:xs)

--Helper functions
overlap::(Ord a) => Range a -> Range a -> Bool
overlap (_,i1h) (i2l, _) = i1h > i2l

disjoint::(Ord a) => Range a -> Range a -> Bool
disjoint (_,i1h) (i2l, _) = i1h < i2l

{-
insertRange::(Ord a) => Range a -> Ranges a -> Ranges a
insertRange ele []     = [ele]
insertRange ele (x:xs) | x < ele   = if overlap x ele 
                                     then insertRange (head (unionRange x ele)) xs
                                     else x:ele:xs
                       | otherwise = x:insertRange ele xs
-}

unionRanges::(Ord a) => Ranges a -> Ranges a -> Ranges a
unionRanges []        r         = r
unionRanges r         []        = r
unionRanges (r1:r1xs) (r2:r2xs) = if r1 < r2
                                  then if overlap r1 r2 
                                       then unionRanges r1xs (unionRange r1 r2 ++ r2xs)
                                       else r1:unionRanges r1xs (r2:r2xs)
                                  else if overlap r2 r1
                                       then unionRanges (unionRange r2 r1 ++ r1xs) r2xs
                                       else r2:unionRanges (r1:r1xs) r2xs

intersectRanges::(Ord a) => Ranges a -> Ranges a -> Ranges a
intersectRanges []        r         = []
intersectRanges r         []        = []
intersectRanges (r1:r1xs) (r2:r2xs) = if r1 < r2
                                      then intersectRange r1 r2++intersectRanges r1xs (r2:r2xs)
                                      else intersectRange r2 r1++intersectRanges (r1:r1xs) r2xs

diffRanges::(Ord a) => Ranges a -> Ranges a -> Ranges a
diffRanges []        r         = []
diffRanges r         []        = r
diffRanges (r1:r1xs) (r2:r2xs) = if r1 < r2
                                 then diffRange r1 r2++diffRanges r1xs (r2:r2xs)
                                 else if overlap r2 r1
                                      then diffRanges (diffRange r2 r1 ++ r1xs) r2xs
                                      else diffRanges (r1:r1xs) r2xs

--Aanname: i1l < i2l
unionRange::(Ord a) => Range a -> Range a -> Ranges a
unionRange (i1l,i1h) (i2l, i2h) = if i1h > i2l --Overlap?
                                  then if i1h > i2h --Complete overlap?
                                       then [(i1l,i1h)] --i1 Encapsulates i2
                                       else [(i1l,i2h)] 
                                  else [(i1l,i1h),(i2l, i2h)] --No overlap
                                  
intersectRange::(Ord a) => Range a -> Range a -> Ranges a
intersectRange (i1l,i1h) (i2l, i2h) =  if i1h > i2l --Overlap?
                                       then if i1h > i2h --Complete overlap?
                                            then [(i2l,i2h)] --i1 Encapsulates i2
                                            else [(i1h,i2l)] 
                                       else [] --No overlap
                                          
diffRange::(Ord a) => Range a -> Range a -> Ranges a
diffRange (i1l,i1h) (i2l, i2h) =  if i1h > i2l --Overlap?
                                  then if i1h > i2h --Complete overlap?
                                       then [(i1l,i2l),(i1h,i2h)] --i1 Encapsulates i2
                                       else [(i1l,i2l)] 
                                  else [(i1l,i1h)] --No overlap                                     

sort2::(Ord a) => a -> a -> (a, a)
sort2 x y = if x < y then (x,y) else (y,x)
