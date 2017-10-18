--------------------------------------------------------------------------------
title:    Java interfaces map to Haskell records
date:     2017 Oct 18
slug:     interfaces-and-records
abstract: A toy example showing a Java-to-Haskell translation in which an
          interface in Java is rewritten as a record in Haskell.
--------------------------------------------------------------------------------

Gabriel Gonzalez recently wrote an article entitled [*Advice for Haskell
beginners*][gabe] and I was mildly surprised to see that out of four tips,
one was:

  [gabe]: http://www.haskellforall.com/2017/10/advice-for-haskell-beginners.html

<p style="text-align: center; margin: 2em auto; font-weight: bold;">
Avoid typeclass abuse
</p>

On second thought, though, this *is* important advice. In retrospect, my
inclination to use typeclasses in inappropriate ways was a big problem in my
earlier Haskell days. I made this mistake a lot:

<p style="text-align: center; margin: 2em auto;">
I thought Java interfaces mapped to Haskell typeclasses.
</p>

<p style="text-align: center; margin: 2em auto;">
But really, more often they map to Haskell <emph>records</emph>.
</p>

[Julie Moronuki and I have written fairly extensively][joy] about the extent to
which interfaces and typeclasses can be compared. (Summary of our conclusion:
they share a related conceptual purpose but are not directly analogous.) Here I
want to share a quick example of how they *do* map to records, by translating a
Java example into Haskell.

  [joy]: https://joyofhaskell.com/posts/2017-03-15-typeclasses-in-translation.html

To pick a simple and arbitrary example, I went to the Oracle's Java
documentation page entitled [*What Is an Interface?*][oracle]. They give the
following `Bicycle` interface:

  [oracle]: https://docs.oracle.com/javase/tutorial/java/concepts/interface.html

```java
interface Bicycle {
    void changeCadence(int newValue);
    void changeGear(int newValue);
    void speedUp(int increment);
    void applyBrakes(int decrement);
}
```

And then a class called `AcmeBicycle`, which implements `Bicycle` and also has
an additional method called `printStates`.

```java
class AcmeBicycle implements Bicycle {

    int cadence = 0;
    int speed = 0;
    int gear = 1;

    void changeCadence(int newValue) {
         cadence = newValue;
    }

    void changeGear(int newValue) {
         gear = newValue;
    }

    void speedUp(int increment) {
         speed = speed + increment;
    }

    void applyBrakes(int decrement) {
         speed = speed - decrement;
    }

    void printStates() {
         System.out.println("cadence:" +
             cadence + " speed:" +
             speed + " gear:" + gear);
    }
}
```

So now let's turn that into Haskell. I'm going to translate as directly as
possible. Using mutable references for the three fields of `AcmeBicycle` is
unusual for Haskell, but for the sake of comparison we'll do it anyway using
three `IORef`s.

The preliminaries: Let's enable `RecordWildCards` because we'll be using records
a lot, and import the module containing `IORef`.

```haskell
{-# LANGUAGE RecordWildCards #-}

import Data.IORef
```

Now we'll jump straight to the punchline: Here's how the `Bicycle` Java
interface is represented as a Haskell record. Each of the four methods in the
interface corresponds to a field in the record.

```haskell
data Bicycle =
  Bicycle
    { changeCadence :: Int -> IO ()
    , changeGear    :: Int -> IO ()
    , speedUp       :: Int -> IO ()
    , applyBrakes   :: Int -> IO ()
    }
```

The `AcmeBicycle` Java class *also* translates into a Haskell record. Each of
the three fields in the Java interface corresponds to a field in the record.

```haskell
data AcmeBicycle =
  AcmeBicycle
    { cadence :: IORef Int
    , speed   :: IORef Int
    , gear    :: IORef Int
    }
```

The implicit constructor in the Java class becomes an explicit `IO` action in
Haskell.

```haskell
newAcmeBicycle :: IO AcmeBicycle
newAcmeBicycle =
  do
    cadence <- newIORef 0
    speed   <- newIORef 0
    gear    <- newIORef 1
    return AcmeBicycle{..}
```

The `printStates` Java class method becomes an ordinary top-level Haskell
function.

```haskell
printStates :: AcmeBicycle -> IO ()
printStates AcmeBicycle{..} =
  do
    c <- readIORef cadence
    s <- readIORef speed
    g <- readIORef gear
    putStrLn ("cadence:" ++ show c ++
              " speed:" ++ show s ++
              " gear:" ++ show g)
```

In the Java example, `AcmeBicycle` is a *subtype* of `Bicycle`, which in Java
parlance means that every `AcmeBicycle` value is *also* a `Bicycle` value. Thus
if `x` is an `AcmeBicycle` and `f` is a function that accepts a `Bicycle`
argument, we can write the Java expression `f(x)`.

We don't have that kind of polymorphism in our Haskell translation, but we
really don't need it; we just need a function that converts `AcmeBicycle` to
`Bicycle`.

```haskell
acmeToBicycle :: AcmeBicycle -> Bicycle
acmeToBicycle AcmeBicycle{..} =
  Bicycle
    { changeCadence = writeIORef cadence
    , changeGear    = writeIORef gear
    , speedUp       = \x -> modifyIORef' speed (\s -> s + x)
    , applyBrakes   = \x -> modifyIORef' speed (\s -> s - x)
    }
```

Thus if `x` is an `AcmeBicycle` and `f` is a function that accepts a `Bicycle`
argument, we can write the Haskell expression `f (acmeToBicycle x)`. There is
one extra function call involved, but in exchange we eliminate the conceptual
complexity of Java subtyping or Haskell overloading, and we can program entirely
in simple monomorphic functions and data types.
