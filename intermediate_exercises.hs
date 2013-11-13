class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry = map

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f Nothing = Nothing
  furry f (Just x) = Just (f x)

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry = (.)

newtype EitherLeft b a  = EitherLeft (Either a b) deriving (Show)
newtype EitherRight a b = EitherRight (Either a b) deriving (Show)

-- Specialized instances:
--   (a -> b) -> (EitherLeft t a) -> (EitherLeft t b)
--   (a -> b) -> (EitherRight t a) -> (EitherRight t a)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left a))  = EitherLeft (Left $ f a)
  furry f (EitherLeft (Right a)) = EitherLeft (Right a)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Right a)) = EitherRight . Right $ f a
  furry f (EitherRight (Left a))  = EitherRight $ Left a
  

class Misty m where
  banana :: (a -> m b) -> m a -> m b
  unicorn :: a -> m a
  -- Exercise 6
  -- Relative Difficulty: 3
  -- (use banana and/or unicorn)
  furry' :: (a -> b) -> m a -> m b
  furry' f = banana (unicorn . f)

-- Exercise 7
-- Relative Difficulty: 2
instance Misty [] where
  banana = concatMap
  unicorn x = [x]

-- Exercise 8
-- Relative Difficulty: 2
instance Misty Maybe where
  banana f Nothing = Nothing
  banana f (Just x) = f x
  unicorn = Just

-- Exercise 9
-- Relative Difficulty: 6
-- Specialized instances:
--    (a -> (t -> b)) -> (t -> a) -> (t -> b)
--    a -> (t -> a)
instance Misty ((->) t) where
  banana f g = (\t -> (f $ g t) t)
  unicorn = const

-- Exercise 10
-- Relative Difficulty: 6
-- (a -> EitherLeft t b) -> (EitherLeft t a) -> (EitherLeft t b)
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Right t)) = EitherLeft (Right t)
  banana f (EitherLeft (Left a))  = f a 
  unicorn x = EitherLeft (Left x)

-- Exercise 11
-- Relative Difficulty: 6
-- (a -> EitherRight t b) -> (EitherRight t a) -> (EitherRight t b)
instance Misty (EitherRight t) where
  banana f (EitherRight (Left t))  = EitherRight (Left t)
  banana f (EitherRight (Right a)) = f a
  unicorn x = EitherRight (Right x)

-- Exercise 12
-- Relative Difficulty: 3
-- this is join
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple = banana . (flip furry')

-- Exercise 14
-- Relative Difficulty: 6
-- cons lifted m b -> m [b] -> m [b]
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy l f = foldr (banana2 (:) . f) (unicorn []) l

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage l = moppy l id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
-- (a -> (b -> c)) -> m a -> m (b -> c)
banana2 = (flip apple . ) . furry'

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
-- m a -> m b -> m (c -> d)
banana3 f = (flip apple .) . banana2 f

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 :: (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
--  m a -> m b -> m c -> m (d -> e)
banana4 f = ((flip apple . ) . ) . banana3 f

newtype State s a = State {
  state :: (s -> (s, a))
}

-- Exercise 19
-- Relative Difficulty: 9
-- Specialized version:
--   furry :: (a -> b) -> (State s a) -> (State s b)
instance Fluffy (State s) where
  furry f s = State $ \s1 ->
      let (newState, val) = state s s1
      in  (newState , f val)

-- Exercise 20
-- Relative Difficulty: 10
-- Specialized version:
--   banana :: (a -> State s b) -> (State s a) -> (State s b)
--  unicorn :: a -> (State s a)
instance Misty (State s) where
  banana f s = State $ \s1 ->
      let (newS, val) = state s s1
          nextState = f val
      in  state nextState newS

  unicorn x = State (\s -> (s,x))
