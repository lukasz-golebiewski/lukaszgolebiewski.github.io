---
layout: post
title:  "Overlappable instances"
date:   2021-01-06 18:20:12 +0100
categories: haskell effects
---
There are many patterns, styles and libraries which can be chosen for dealing with effects in Haskell. Today I'll try to bring closer one of them.

Let's start off with a very simple program written using tagless final style.
The program inserts a value "1" into the store under the key "key1" and then tries to retrieve it using operations defined in the `Store` type class. It logs what is happening while doing the above thanks to the `Logger`.
The instances needed for `IO` are added only to make the whole thing compile, the implementations aren't very useful (for now!)


```haskell
{-# LANGUAGE DerivingStrategies,UndecidableInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Effects.Overlapping where

import Prelude hiding (log)

class Store m where
  put :: k -> a -> m ()
  get :: k -> m (Maybe a)

class Logger m where
  log :: msg -> m ()

program ::
     Monad m
  => Logger m
  => Store m
  => m ()
program = do
  put key1 val1
  log $ "Inserted value: " <> show val1 <> " under key: " <> key1
  maybeV <- get key1
  case maybeV of
    Just v -> log $ "Retrieved: " <> v
    Nothing -> log "No data found"
  where
    key1 = "key1"
    val1 :: Integer
    val1 = 1

instance Store IO where
  put _ _ = pure ()
  get _ = pure Nothing

instance Logger IO where
  log _ = pure ()

main :: IO ()
main = program
```
This is clearly limiting, because we are allowed to have only one typeclass instance for a given monad transformer. If we would like to have a NoOp instance and a real-world instance, we have to use newtype wrappers. Let's add aliases then for our not-so-useful instances like this:

```haskell
newtype NoOpStoreT m a = NoOpStoreT { runNoOpStoreT :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Monad m => Store (NoOpStoreT m) where
  put _ _ = pure ()
  get _ = pure Nothing

newtype NoOpLoggerT m a = NoOpLoggerT { runNoOpLoggerT :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Monad m => Logger (NoOpLoggerT m) where
  log _ = pure ()

main :: IO ()
main = runNoOpStoreT . runNoOpLoggerT $ program
```
Compilation fails with the following error:
```
    • No instance for (Store (NoOpLoggerT (NoOpStoreT IO)))
        arising from a use of ‘program’
```
Okay then, let's add what the compiler is asking for
```haskell
instance Monad m => Store (NoOpLoggerT m) where
  put _ _ = pure ()
  get _ = pure Nothing
```
This compiles and runs fine. But let's see what happens if we reverse the application of transformers:
```haskell
main :: IO ()
main = runNoOpLoggerT . runNoOpStoreT $ program
```
results in:
```
    • No instance for (Logger (NoOpStoreT (NoOpLoggerT IO)))
        arising from a use of ‘program’
```
After adding it in a similar manner we'll arrive at the infamous O(n^2) instances problem!
```haskell
instance Monad m => Logger (NoOpStoreT m) where
  log _ = pure ()
```
This gets painful really quickly when we add more classes and transformers.
Overlappable transformer instances to the rescue! Adding these both for `Store` and `Logger` makes it possible to get rid of boilerplate and allow us to stack our transformers in any[^1] order! Let's start with `Logger`:

```haskell
instance MonadTrans NoOpStoreT where
  lift a = NoOpStoreT a

instance (Logger m, MonadTrans t, Monad m) => Logger (t m) where
  log a = lift $ log a

instance Monad m => Logger (NoOpLoggerT m) where
  log _ = pure ()
```
We've defined a new instance. It is going to be available only if when a `Logger` instance already exists for `m` and we know how to wrap this monad `m` using `t`. For this to be true we need to add a `MonadTrans` instance for our `NoOpStoreT`. Thanks to this we don't have to write all n^2 instances by hand. But the compiler has a problem now:

```
   • Overlapping instances for Logger (NoOpLoggerT IO)
        arising from a use of ‘program’
      Matching instances:
        instance (Logger m, MonadTrans t, Monad m) => Logger (t m)
          -- Defined at ...
        instance Monad m => Logger (NoOpLoggerT m)
          -- Defined at ...

```
We need to tell it, that whenever two instances could be used "arising from the use of program", one of them is less specific, and labeled using the `{-# OVERLAPPABLE #-}` pragma. Now the code compiles fine.

Let's do the same for `Store` thus arriving at:
```haskell
newtype NoOpStoreT m a = NoOpStoreT { runNoOpStoreT :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Monad m => Store (NoOpStoreT m) where
  put _ _ = pure ()
  get _ = pure Nothing

instance {-# OVERLAPPABLE #-} (Store m, MonadTrans t, Monad m) => Store (t m) where
  put a b = lift $ put a b
  get a = lift $ get a

instance MonadTrans NoOpStoreT where
  lift a = NoOpStoreT a


newtype NoOpLoggerT m a = NoOpLoggerT { runNoOpLoggerT :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance {-# OVERLAPPABLE #-} (Logger m, MonadTrans t, Monad m) => Logger (t m) where
  log a = lift $ log a

instance MonadTrans NoOpLoggerT where
  lift a = NoOpLoggerT a

instance Monad m => Logger (NoOpLoggerT m) where
  log _ = pure ()

```
Now we can run the effects in any[^1] order. Both:

```haskell
main :: IO ()
main = runNoOpLoggerT . runNoOpStoreT $ program
```
and
```haskell
main :: IO ()
main = runNoOpStoreT . runNoOpLoggerT $ program
```
compile and run just fine.

Now let's add some more useful instances for our classes. An implementation of a `Store` backed by a `Map` and a `Logger` capable of writing to stdout. The classes and the program had to be modified a bit in order to make the whole thing compile with the backing `Map` and `MonadState`. Luckily the existing instances didn't need to be modified much and the end result looks like this:

```haskell
module Effects.Overlapping where

import Prelude hiding (log, lookup)

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.Trans.State (evalStateT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State.Class qualified as MS (MonadState, modify, get)
import Data.Map.Strict (Map, empty, insert, lookup)

class Store m k v where
  put :: k -> v -> m ()
  get :: k -> m (Maybe v)

class Logger m a where
  log :: a -> m ()

program ::
     Monad m
  => Logger m String
  => Store m String Integer
  => m ()
program = do
  put key1 val1
  log $ "Inserted value: " <> show val1 <> " under key: " <> key1
  maybeV <- get key1
  case maybeV :: Maybe Integer of
    Just val -> log $ "Retrieved: " <> show val
    Nothing -> log "No data found"
  where
    key1 :: String
    key1 = "key1"
    val1 :: Integer
    val1 = 1

----------------------------------------------------------------------------------------
newtype NoOpStoreT m a = NoOpStoreT { runNoOpStoreT :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Monad m => Store (NoOpStoreT m) k v where
  put _ _ = pure ()
  get _ = pure Nothing

instance {-# OVERLAPPABLE #-} (Store m k v, MonadTrans t, Monad m) => Store (t m) k v where
  put a b = lift $ put a b
  get a = lift $ get a

instance MonadTrans NoOpStoreT where
  lift a = NoOpStoreT a

newtype StoreT m a = StoreT { runStoreT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MS.MonadState s)

instance (Ord k, MS.MonadState (Map k v) m, Monad m) => Store (StoreT m) k v where
  put key value = MS.modify (insert key value)
  get key = fmap (lookup key) MS.get

instance MonadTrans StoreT where
  lift a = StoreT a

----------------------------------------------------------------------------------------
newtype NoOpLoggerT m a = NoOpLoggerT { runNoOpLoggerT :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance {-# OVERLAPPABLE #-} (Logger m a, MonadTrans t, Monad m) => Logger (t m) a where
  log a = lift $ log a

instance MonadTrans NoOpLoggerT where
  lift a = NoOpLoggerT a

instance Monad m => Logger (NoOpLoggerT m) a where
  log _ = pure ()

newtype LoggerT m a = LoggerT { runLoggerT :: m a }
  deriving newtype (Functor, Applicative, Monad, MonadIO, MS.MonadState s)

instance MonadTrans LoggerT where
  lift a = LoggerT a

instance (Show a, MonadIO m) => Logger (LoggerT m) a where
  log msg = liftIO $ putStrLn (show msg)

runMapStoreT = flip evalStateT (empty :: Map String Integer) . runStoreT

main :: IO ()
main = do
  putStrLn "Running the program..."
  runMapStoreT . runLoggerT $ program
  putStrLn "Done"

```
Now we can do some more useful things with our program and stack transformers according to our needs.
Hope you enjoyed the read :-)

[^1]: Actually, almost any. There are nuances here which are out of scope of this blog post.
