module Data.Range where

type Range a = (a,a)
type Ranges a = [Range a]

-- * Invariants

-- | Function to determine whether a range interval is valid. This means the
-- first part of the tuple should be lower or equal to the second part
-- of the tuple
rangeOK::(Ord a) => Range a -> Bool
rangeOK (l,h) = l <= h

-- | Function to determine whether a list of consecutive ranges is valid. This means
-- the intervals are disjoint, and are ordered in increasing order.
rangesOK::(Ord a) => Ranges a -> Bool
rangesOK []         = True
rangesOK [r]        = rangeOK r
rangesOK (r1:r2:xs) = rangeOK r1 && r1 < r2 && disjoint r1 r2 && rangesOK (r2:xs)

-- * Helper functions

-- | Checks wheter two ranges overlap, demands that i1 < i2.
overlap::(Ord a) => Range a -> Range a -> Bool
overlap (_,i1h) (i2l, _) = i1h > i2l

-- | Checks wheter two ranges are disjoint, demands that i1 < i2.
disjoint::(Ord a) => Range a -> Range a -> Bool
disjoint (_,i1h) (i2l, _) = i1h < i2l

-- | Determines the entire domain spanned by a list of ranges.
span::(Ord a) => Ranges a -> Range a
span ls = (fst.head $ ls, snd.last $ ls)

-- | Determines the complement of a set of ranges. Des not include the outer intervals.
complement::(Ord a) => Ranges a -> Ranges a
complement [] = []
complement [_] = []
complement ((xl,xh):(yl,yh):ls) = (xh,yl):complement ((yl,yh):ls)

-- | Determines the complement within a certain area.
complementIn::(Ord a) => Range a -> Ranges a -> Ranges a
complementIn (lbound,hbound) ls = (lbound,fst.head $ ls):complement ls ++ [(snd.last $ ls, hbound)]

-- * Combination functions

-- | Inserts a range into a list of ranges. Reequires that the ranges are valid.
insertRange::(Ord a) => Range a -> Ranges a -> Ranges a
insertRange ele []     = [ele]
insertRange ele@(el,eh) (x@(xl,xh):xs) 
  | xh >= el  = f                                 -- ele entering domain?
  | otherwise = x:insertRange ele xs
  where f | el < xl   = g                         -- ele entering domain?
          | eh < xh   = x:xs                      -- e completely inside x 
          | otherwise = insertRange (xl, eh) xs   -- partial overlap
        g | eh < xl   = ele:x:xs                  -- no overlap; insert
          | eh < xh   = (el,xh):xs                -- partial overlap
          | otherwise = insertRange (el, eh) xs   -- x completely inside e

-- | Takes the union of 2 sets of ranges.
unionRanges::(Ord a) => Ranges a -> Ranges a -> Ranges a
unionRanges r              []             = r
unionRanges []             r              = r  
unionRanges (x@(xl,xh):xs) (y@(yl,yh):ys) 
  | xl < yl = f                                   -- overlap?
  | yh < xl = y:unionRanges (x:xs) ys             -- no overlap
  | yh > xh = unionRanges xs (y:ys)               -- x fully in y?
  | otherwise = unionRanges ((yl,xh):xs) ys
  where f | xh < yl = x : unionRanges xs (y:ys)   -- no overlap 
          | xh > yh = unionRanges (x:xs) ys 
          | otherwise = unionRanges xs ((xl, yh):ys)

-- | Takes the intersection of 2 sets of ranges.
intersectRanges::(Ord a) => Ranges a -> Ranges a -> Ranges a
intersectRanges []             _              = []
intersectRanges _              []             = []
intersectRanges (x@(xl,xh):xs) (y@(yl,yh):ys) 
  | xl < yl   = f                                   --overlap?
  | yh < xl   = intersectRanges (x:xs) ys           --no overlap
  | yh > xh   = x:intersectRanges xs (y:ys)         -- x fully in y
  | otherwise = (xl,yh):intersectRanges (x:xs) ys
  where f | xh < yl   = intersectRanges xs (y:ys)   -- no overlap 
          | xh > yh   = y:intersectRanges (x:xs) ys 
          | otherwise = (yl,xh):intersectRanges xs (y:ys)

-- | Takes the difference of 2 sets of ranges.
diffRanges::(Ord a) => Ranges a -> Ranges a -> Ranges a
diffRanges []             _              = []
diffRanges r              []             = r
diffRanges (x@(xl,xh):xs) (y@(yl,yh):ys) 
  | xl <  yl  = f                                          -- x lowest ?
  | yh <  xl  = diffRanges (x:xs) ys                       -- no overlap
  | yh >= xh  = diffRanges xs (y:ys)                       -- x fully in y
  | otherwise = diffRanges ((yh,xh):xs) ys
  where f | xh <= yl   = x:diffRanges xs (y:ys)             -- no overlap
          | xh >= yh  = (xl,yl):diffRanges ((yh,xh):xs) ys -- y fully in x
          | otherwise = (xl,yl):diffRanges xs (y:ys)
