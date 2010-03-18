module Data.Range where
import Data.Set

type Range a = (a,a)
type Ranges a = [Range a]

--Invariants
rangeOK::(Ord a) => Range a -> Bool
rangeOK (l,h) = l <= h

rangesOK::(Ord a) => Ranges a -> Bool
rangesOK []         = True
rangesOK [r]        = rangeOK r
rangesOK (r1:r2:xs) = rangeOK r1 && r1 < r2 && disjoint r1 r2 && rangesOK (r2:xs)

--Helper functions, aanname: i1 < i2
overlap::(Ord a) => Range a -> Range a -> Bool
overlap (_,i1h) (i2l, _) = i1h > i2l

disjoint::(Ord a) => Range a -> Range a -> Bool
disjoint (_,i1h) (i2l, _) = i1h < i2l

span::(Ord a) => Ranges a -> Range a
span ls = (fst.head $ ls, snd.last $ ls)

complement::(Ord a) => Ranges a -> Ranges a
complement [] = []
complement [_] = []
complement ((xl,xh):(yl,yh):ls) = (xh,yl):complement ((yl,yh):ls)

complementIn::(Ord a) => Range a -> Ranges a -> Ranges a
complementIn (lbound,hbound) ls = (lbound,fst.head $ ls):complement ls ++ [(snd.last $ ls, hbound)]

--Requires valid range/ranges
insertRange::(Ord a) => Range a -> Ranges a -> Ranges a
insertRange ele []     = [ele]
insertRange ele@(el,eh) (x@(xl,xh):xs) | xh >= el --Ele entering domain?
                                                  = if el < xl --Ele before x?
                                                    then if eh < xl --Overlap?
                                                         then ele:x:xs -- no overlap, insert
                                                         else if eh < xh --eh inside?
                                                              then (el,xh):xs --partial overlap
                                                              else insertRange (el,eh) xs --x completely inside e
                                                    else --el inside x
                                                         if eh < xh
                                                         then x:xs--e completely inside x
                                                         else insertRange (xl,eh) xs --partial overlap
                                       | otherwise = x:insertRange ele xs

unionRanges::(Ord a) => Ranges a -> Ranges a -> Ranges a
unionRanges r              []             = r
unionRanges []             r              = r  
unionRanges (x@(xl,xh):xs) (y@(yl,yh):ys) = if xl < yl --x lowest ?
                                            then if xh < yl --overlap?
                                                 then x:unionRanges xs (y:ys) --no overlap
                                                 else if xh > yh --y fully in x?
                                                      then unionRanges (x:xs) ys
                                                      else unionRanges xs ((xl,yh):ys)
                                            else if yh < xl --overlap?
                                                 then y:unionRanges (x:xs) ys --no overlap
                                                 else if yh > xh --x fully in y?
                                                      then unionRanges xs (y:ys)
                                                      else unionRanges ((yl,xh):xs) ys

intersectRanges::(Ord a) => Ranges a -> Ranges a -> Ranges a
intersectRanges []             _              = []
intersectRanges _              []             = []
intersectRanges (x@(xl,xh):xs) (y@(yl,yh):ys) = if xl < yl --x lowest ?
                                                then if xh < yl --overlap?
                                                     then intersectRanges xs (y:ys) --no overlap
                                                     else if xh > yh --y fully in x?
                                                          then y:intersectRanges (x:xs) ys 
                                                          else (yl,xh):intersectRanges xs (y:ys)
                                                else if yh < xl --overlap?
                                                     then intersectRanges (x:xs) ys --no overlap
                                                     else if yh > xh --x fully in y?
                                                          then x:intersectRanges xs (y:ys) 
                                                          else (xl,yh):intersectRanges (x:xs) ys

diffRanges::(Ord a) => Ranges a -> Ranges a -> Ranges a
diffRanges []             _              = []
diffRanges r              []             = r
diffRanges (x@(xl,xh):xs) (y@(yl,yh):ys) = if xl <= yl --x lowest ?
                                           then if xh < yl --overlap?
                                                then x:diffRanges xs (y:ys) --no overlap
                                                else if xh >= yh --y fully in x?
                                                     then (xl,yl):diffRanges ((yh,xh):xs) ys 
                                                     else (xl,yl):diffRanges xs (y:ys)
                                                else if yh < xl --overlap?
                                                     then diffRanges (x:xs) ys --no overlap
                                                     else if yh >= xh --x fully in y?
                                                          then diffRanges xs (y:ys) 
                                                          else diffRanges ((yh,xh):xs) ys

{-
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
                                            else [(i2l,i1h)] 
                                       else [] --No overlap
                                          
diffRange::(Ord a) => Range a -> Range a -> Ranges a
diffRange (i1l,i1h) (i2l, i2h) =  if i1h > i2l --Overlap?
                                  then if i1h > i2h --Complete overlap?
                                       then [(i1l,i2l),(i1h,i2h)] --i1 Encapsulates i2
                                       else [(i1l,i2l)] 
                                  else [(i1l,i1h)] --No overlap                                     
-}
