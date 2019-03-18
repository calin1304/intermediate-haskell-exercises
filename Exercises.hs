class Fluffy f where
  furry :: (a -> b) -> f a -> f b

-- Exercise 1
-- Relative Difficulty: 1
instance Fluffy [] where
  furry f = foldr (\a b -> f a : b) []

-- Exercise 2
-- Relative Difficulty: 1
instance Fluffy Maybe where
  furry f Nothing = Nothing
  furry f (Just x) = Just (f x)

-- Exercise 3
-- Relative Difficulty: 5
instance Fluffy ((->) t) where
  furry f g = (\x -> f (g x))

newtype EitherLeft b a =
  EitherLeft (Either a b)

newtype EitherRight a b =
  EitherRight (Either a b)

-- Exercise 4
-- Relative Difficulty: 5
instance Fluffy (EitherLeft t) where
  furry f (EitherLeft (Left x)) = EitherLeft (Left (f x))
  furry f (EitherLeft (Right x)) = EitherLeft (Right x)

-- Exercise 5
-- Relative Difficulty: 5
instance Fluffy (EitherRight t) where
  furry f (EitherRight (Left x)) = EitherRight (Left x)
  furry f (EitherRight (Right x)) = EitherRight (Right (f x))

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
instance Misty ((->) t) where
    -- banana :: (a -> (-> t) b) -> ((-> t) a) -> (->t) b
    -- banana :: (a -> t -> b) -> (t -> a) -> (t -> b)
  banana f g = (\t -> f (g t) t)
  unicorn x = (\_ -> x)

-- Exercise 10
-- Relative Difficulty: 6
instance Misty (EitherLeft t) where
  banana f (EitherLeft (Left x)) = f x
  banana f (EitherLeft (Right x)) = EitherLeft (Right x)
  unicorn x = EitherLeft (Left x)

-- Exercise 11
-- Relative Difficulty: 6
instance Misty (EitherRight t) where
  banana f (EitherRight (Right x)) = f x
  banana f (EitherRight (Left x)) = EitherRight (Left x)
  unicorn x = EitherRight (Right x)

-- Exercise 12
-- Relative Difficulty: 3
jellybean :: (Misty m) => m (m a) -> m a
jellybean = banana id

-- Exercise 13
-- Relative Difficulty: 6
apple :: (Misty m) => m a -> m (a -> b) -> m b
apple k = banana (\f -> banana (unicorn . f) k)

-- Exercise 14
-- Relative Difficulty: 6
moppy :: (Misty m) => [a] -> (a -> m b) -> m [b]
moppy []     _ = unicorn []
moppy (x:xs) k = (\b -> (unicorn . (\bs -> b : bs)) `banana` (moppy xs k)) `banana` (k x)

-- Exercise 15
-- Relative Difficulty: 6
-- (bonus: use moppy)
sausage :: (Misty m) => [m a] -> m [a]
sausage xs = moppy xs id

-- Exercise 16
-- Relative Difficulty: 6
-- (bonus: use apple + furry')
banana2 :: (Misty m) => (a -> b -> c) -> m a -> m b -> m c
banana2 f a b = b `apple` furry' f a

-- Exercise 17
-- Relative Difficulty: 6
-- (bonus: use apple + banana2)
banana3 :: (Misty m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
banana3 f a b c = c `apple` banana2 f a b

-- Exercise 18
-- Relative Difficulty: 6
-- (bonus: use apple + banana3)
banana4 ::
     (Misty m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
banana4 f a b c d = d `apple` banana3 f a b c

newtype State s a = State
  { state :: s -> (s, a)
  }

-- Exercise 19
-- Relative Difficulty: 9
instance Fluffy (State s) where
  furry f a = State (\s -> let (s', x) = state a s in (s', f x))

-- Exercise 20
-- Relative Difficulty: 10
instance Misty (State s) where
  banana f m =
    State
      (\s ->
         let (s', x) = state m s
             (s'', x') = state (f x) s'
          in (s'', x'))
  unicorn x = State (\s -> (s, x))