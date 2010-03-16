-- | Provides function to convert degrees to radians. Nothing more.
--
module Data.Radians where

radians :: (Floating a) => a -> a
radians n = n / (360 / (2 * pi))
