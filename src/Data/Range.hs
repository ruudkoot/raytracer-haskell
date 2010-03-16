module Data.Range where
import Data.Set

type Range a = (a,a)
type Ranges a = [Range a]

unionRanges::(Ord a) => Ranges a -> Ranges a -> Ranges a
unionRanges = foldr insertRange

overlap::(Ord a) => Range a -> Range a -> Bool
overlap (_,i1h) (i2l, _) = i1h > i2l

insertRange::(Ord a) => Range a -> Ranges a -> Ranges a
insertRange ele []     = [ele]
insertRange ele (x:xs) | x < ele   = if overlap x ele 
                                     then insertRange (head (unionRange x ele)) xs
                                     else x:ele:xs
                       | otherwise = x:insertRange ele xs

--Aanname: i1l < i2l
unionRange::(Ord a) => Range a -> Range a -> Ranges a
unionRange (i1l,i1h) (i2l, i2h) = if i1h > i2l --Overlap?
                                  then if i1h > i2h --Complete overlap?
                                       then [(i1l,i1h)] --i1 Encapsulates i2
                                       else [(i1l,i2h)] 
                                  else [(i1l,i1h),(i2l, i2h)] --No overlap
                                  
intersectionRange::(Ord a) => Range a -> Range a -> Ranges a
intersectionRange (i1l,i1h) (i2l, i2h) =  if i1h > i2l --Overlap?
                                          then if i1h > i2h --Complete overlap?
                                               then [(i2l,i2h)] --i1 Encapsulates i2
                                               else [(i1h,i2l)] 
                                          else [] --No overlap
                                          
differenceRange::(Ord a) => Range a -> Range a -> Ranges a
differenceRange (i1l,i1h) (i2l, i2h) =  if i1h > i2l --Overlap?
                                        then if i1h > i2h --Complete overlap?
                                             then [(i1l,i2l),(i1h,i2h)] --i1 Encapsulates i2
                                             else [(i1l,i2l)] 
                                        else [(i1l,i1h)] --No overlap                                     
