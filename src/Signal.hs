-- A very simple library for manipulating continuous signals.
--

module Signal (Time, Signal,      -- Types
               constS, timeS,     -- Constructors
               ($$), mapT, mapS,  -- Combinators
               sample )           -- Semantic function
       where
import Control.Monad (forM_)

-- Smart constructors

-- Constant signal
constS :: a -> Signal a
constS x = Sig (const x)

-- Time varying signal
timeS  ::      Signal Time
timeS = Sig id

-- Combinators

-- Function application lifted to signals.
($$)   :: Signal (a -> b) -> Signal a -> Signal b
fs $$ xs = Sig (\t -> unSig fs t  (unSig xs t))

-- Transforming the time.
mapT   :: (Time -> Time)  -> Signal a -> Signal a
mapT f xs = Sig (unSig xs . f)

-- Mapping a function over a signal.
mapS   :: (a -> b)        -> Signal a -> Signal b
mapS f xs = constS f $$ xs

-- Sampling a signal at a given time point.
-- This is the /semantic function/ of our library.
sample :: Signal a -> Time -> a  
sample = unSig

{-------}

type Time = Double
newtype Signal a = Sig {unSig :: Time -> a}


--------------------------------------------
-- Example

-- sinusoidal of given frequency
sinS :: Double -> Signal Double
sinS freq = mapT (freq*) $ mapS sin timeS


scale :: Num a =>  Signal a -> Signal a
scale = mapS ((30*) . (1+))

-- Discretize a signal
discretize :: Signal Double -> Signal Int
discretize = mapS round

-- convert to "analog"
toBars :: Signal Int -> Signal String
toBars = mapS (`replicate` '#') 

displayLength = 500
-- display the signal at a number of points
display :: Signal String -> IO ()
display ss = forM_ [0..displayLength] $ \x ->
   putStrLn (sample ss x)

-- The display magic.
-- Note how we take advantage of function composition, 
-- types defined so far, etc.
magic :: Signal Double -> IO ()
magic = display . toBars . discretize . scale

test1 :: IO ()
test1 = magic $ sinS 0.1
