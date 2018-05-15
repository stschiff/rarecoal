module Rarecoal.Powell (powell, powellV) where

import Control.Monad.IO.Class (liftIO, MonadIO)
import qualified Data.Vector.Unboxed as V
import Debug.Trace (trace)
import Numeric.GSL.Minimization (uniMinimize, UniMinimizeMethod(..))
import Numeric.LinearAlgebra.Data (Vector, R, scalar, toLists, size, toColumns, ident, 
    fromList, toList)
import System.IO (hPutStrLn, stderr)

gold :: Double
gold = 1.618034

glimit :: Double
glimit = 100.0

tiny :: Double
tiny = 1.0e-20

sign :: Double -> Double -> Double
sign a b = if b >= 0 then (if a >= 0 then a else -a) else (if a >= 0 then -a else a)

-- From Numerical Recipes 3rd edition, page 491
bracket :: Double -> Double -> (Double -> Double) -> (Double, Double, Double)
bracket a b func = 
    let fa' = func a
        fb' = func b
        (fa, fb) = if fb' > fa' then (fb', fa') else (fa', fb')
        (ax, bx) = if fb' > fa' then (b, a) else (a, b)
        cx = bx + gold * (bx - ax)
        fc = func cx
    in  let ret@((r_ax, r_fa), (r_bx, r_fb), (r_cx, r_fc)) = go (ax, fa) (bx, fb) (cx, fc)
        in  if r_fa < r_fb || r_fc < r_fb
            then
                error ("Error(1) in Bracketing method. Result: " ++ show ret)
            else
                if r_ax <= r_bx && r_bx <= r_cx
                -- then trace (show ("bracket result " ++ show ret)) (r_ax, r_bx, r_cx)
                then (r_ax, r_bx, r_cx)
                else if r_cx <= r_bx && r_bx <= r_ax
                     -- then trace (show ("bracket result " ++ show ret)) (r_cx, r_bx, r_ax)
                     then (r_cx, r_bx, r_ax)
                     else
                         error ("Error(2) in Bracketing method. Result: " ++ show ret)
                
  where
    go (ax, fa) (bx, fb) (cx, fc) 
        | fb <= fc = ((ax, fa), (bx, fb), (cx, fc)) 
        | otherwise =
            -- use parabolic extrapolation to make a guess at u:
            let r = (bx - ax) * (fb - fc)
                q = (bx - cx) * (fb - fa)
                u = bx - ((bx - cx) * q - (bx - ax) * r) /
                    (2.0 * sign (max (abs (q - r)) tiny) (q - r))
                ulim = bx + glimit * (cx - bx)
            in  if (bx - u) * (u - cx) > 0.0 -- u is between b and c
                then
                    let fu = func u
                    in  if fu < fc -- Got minimum between b and c
                        then ((bx, fb), (u, fu), (cx, fc))
                        else if fu > fb -- Got minimum between a and u
                             then ((ax, fa), (bx, fb), (u, fu))
                             else
                                 -- Maginify and keep on moving downhill
                                 let u' = cx + gold * (cx - bx)
                                     fu' = func u
                                 in  go (bx, fb) (cx, fc) (u', fu')
                else
                    if (cx - u) * (u - ulim) > 0.0 -- u is between c and limit
                    then
                        let fu = func u
                        in  if fu < fc
                            then
                                let u' = u + gold * (u - cx)
                                    fu' = func u'
                                in  go (cx, fc) (u, fu) (u', fu')
                            else go (bx, fb) (cx, fc) (u, fu)
                    else
                        if (u - ulim) * (ulim - cx) >= 0.0 -- u is beyond limit
                        then
                            let u' = ulim
                                fu' = func u'
                            in  go (bx, fb) (cx, fc) (u', fu')
                        else -- reject parabolic u and use default maginification
                            let u' = cx + gold * (cx - bx)
                                fu' = func u'
                            in  go (bx, fb) (cx, fc) (u', fu')

bracket0 :: (Double -> Double) -> (Double, Double, Double)
bracket0 = bracket 0.0 1.0

brent0 :: Double -> Int -> (Double -> Double) -> (Double, [(Int, Double, Double, Double)])
brent0 ftol maxIter func =
    let (a, b, c) = bracket0 func
        (minArg, trace) = uniMinimize BrentMini ftol maxIter func b a c
        traceList = do
            [iter, guess, lower, upper] <- toLists trace
            return (floor iter, guess, lower, upper)
    in  (minArg, traceList)

f1dim :: Vector R -> Vector R -> (Vector R -> Double) -> (Double -> Double)
f1dim point0 direction vecFunc x = 
    let point = point0 + scalar x * direction
    -- in  trace ("evaluating vecFunc at point " ++ show point ++ ", val=" ++ show (vecFunc point)) (vecFunc point)
    in  vecFunc point
    
linmin :: Double -> Int -> Vector R -> Vector R -> (Vector R -> Double) -> (Vector R, Double)
linmin ftol maxIter point0 direction vecFunc =
    let (xMin, _) = trace ("calling brent0 from point " ++ show point0 ++ " into direction " ++ show direction) (brent0 ftol maxIter (f1dim point0 direction vecFunc))
    -- let (xMin, _) = brent0 ftol maxIter (f1dim point0 direction vecFunc)
        minPoint = point0 + scalar xMin * direction
        minVal = vecFunc minPoint
    in  trace ("xMin=" ++ show xMin ++ ", minVal=" ++ show minVal) (minPoint, minVal)

powellIter :: (Vector R -> Double) -> Vector R -> Double -> [Vector R] ->
    (Vector R, Double, [Vector R])
powellIter vecFunc point val directions =
    let (newPoint, newVal, deltas) = minimizeAllDirections point val directions
        extrapolatedPoint = scalar 2.0 * newPoint - point
        extrapolatedVal = vecFunc extrapolatedPoint
        newDirection = newPoint - point
        (maxDelta, maxDeltaIndex) = maximum $ zip deltas [0..]
    in  if extrapolatedVal < val
        then
            let t = 2.0 * (val - 2.0 * newVal + extrapolatedVal) *
                    (val - newVal - maxDelta)^^(2::Int) -
                    maxDelta * (val - extrapolatedVal)^^(2::Int)
            in  if t < 0.0
                then
                    let (finalPoint, finalVal) = linmin 3e-8 30 newPoint newDirection vecFunc
                        newDirections =
                            let (ys, zs) = splitAt maxDeltaIndex directions
                            in  ys ++ (tail zs) ++ [newDirection]
                    in  (finalPoint, finalVal, newDirections)
                else (newPoint, newVal, directions)
        else (newPoint, newVal, directions)
  where
    minimizeAllDirections :: Vector R -> Double -> [Vector R] ->
        (Vector R, Double, [Double])
    minimizeAllDirections point' val' directions' = go point' val' directions' []
      where
        go p v [] deltas = (p, v, deltas)
        go p v (xi:rest) deltas = 
            let (minPoint, minVal) = linmin 0.01 30 p xi vecFunc
            in  go minPoint minVal rest (deltas ++ [minVal - v])

powell :: (MonadIO m) => Double -> Int -> (Vector R -> Double) -> Vector R ->
    m (Vector R, Double, [Vector R])
powell ftol maxIter vecFunc p0 = do
    let val0 = vecFunc p0
        dir0 = toColumns $ ident (size p0)
    go 1 [] p0 val0 dir0
  where
    go i trace p val dir = do
        if (i > maxIter) then do
            liftIO $ hPutStrLn stderr "Powell's Method exceeding maximum number of iterations"
            return (p, val, trace)
        else do
            liftIO $ hPutStrLn stderr ("Powell's method (Iteration " ++ show i ++
                "): Minimizing from point " ++ show p ++ " at function value " ++ show val)
            let (p1, val1, dir1) = powellIter vecFunc p val dir
            -- if (2.0 * (val - val1) <= ftol * (abs val + abs val1) + 1.0e-25)
            if val - val1 <= ftol
                then return (p1, val1, trace)
                else
                    let newTrace = trace ++ [fromList (val1:toList p1)]
                    in  go (i + 1) newTrace p1 val1 dir1

type VecDoub = V.Vector Double
powellV :: (MonadIO m) => Double -> Int -> (VecDoub -> Double) -> VecDoub ->
    m (VecDoub, Double, [VecDoub])
powellV ftol maxIter vecFunc p0 = do
    let vecFunc' = vecFunc . V.fromList . toList
        p0' = fromList . V.toList $ p0
    (minPoint, minVal, trace) <- powell ftol maxIter vecFunc' p0'
    let minPoint' = V.fromList . toList $ minPoint
        trace' = map (V.fromList . toList) trace
    return (minPoint', minVal, trace')
    
    
    
    