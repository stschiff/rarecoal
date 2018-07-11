#!/usr/bin/env stack
-- stack runghc

{-# LANGUAGE BangPatterns, OverloadedStrings #-}

import Control.Monad (when)
import Turtle

maxA = 20
maxX = 2
main = stdout $ do
    aP <- select [maxA,(maxA-1)..1]
    a <- select [aP-1,(aP-2)..1]
    let maxX' = min maxX aP
    xP <- select [maxX',(maxX'-1)..1]
    x <- select [xP,(xP-1)..1]
    let r = rFac x a xP aP
    True <- return $ r > 0
    return . unsafeTextToLine $ format ("rFac "%d%" "%d%" "%d%" "%d%" = "%g) x a xP aP r


rFac :: Int -> Int -> Int -> Int -> Double
rFac !x !a !xP !aP | x == xP && a == aP = 1
                   | aP < a = error "rFac called with illegal configuration"
                   | xP < x || (aP - xP) < (a - x) = 0
                   | otherwise = term1 * rFac x a (xP - 1) (aP - 1) + term2 * rFac x a xP (aP - 1)
  where
    term1 = fromIntegral (xP * (xP - 1)) / fromIntegral (aP * (aP - 1))
    term2 = fromIntegral ((aP - xP) * (aP - xP - 1)) / fromIntegral (aP * (aP - 1))
