module Test.Trial.Laws
    ( lawsSpec
      -- * Internals
    , checkAssotiativityFor
    ) where

import Control.Applicative (Alternative (..), liftA2)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Semigroup (..))
import Hedgehog (Gen, forAll, forAllWith, (===))
import Test.Hspec (Spec, describe, it, parallel)
import Test.Hspec.Hedgehog (hedgehog)

import Test.Trial.Gen (Property, genBindFunction, genFunction, genFunction2, genInt, genSmallInt,
                       genSmallList, genTrial)
import Trial (Trial)


lawsSpec :: Spec
lawsSpec = describe "Trial Instance Laws" $ parallel $ do
    describe "Semigroup instance for Trial" $ do
        it "Associativity: a <> (b <> c) ≡ (a <> b) <> c"
            semigroupAssociativity
        it "Concatenation: sconcat ≡ foldr1 (<>)"
            semigroupConcatenation
        it "Times: stimes n a ≡ foldr1 (<>) (replicate n a)"
            semigroupTimes
    describe "Functor instance for Trial" $ do
        it "Identity: fmap id ≡ id"
            functorIdentity
        it "Composition: map f . fmap g ≡ fmap (f . g)"
            functorComposition
        it "Const: fmap (const x) ≡ x <$"
            functorConst
    describe "Applicative instance for Trial" $ do
        it "Functor and Applicative correspondence: fmap f x ≡ pure f <*> x"
            functorApplicative
        it "Identity: pure id <*> x ≡ x"
            applicativeIdentity
        it "Composition: pure (.) <*> f <*> g <*> x ≡ f <*> (g <*> x)"
            applicativeComposition
        it "Homomorphism: pure f <*> pure x ≡ pure (f x)"
            applicativeHomomorphism
        it "Interchange: f <*> pure x ≡ pure ($ x) <*> f"
            applicativeInterchange
        it "Apply Right: u *> v ≡ (id <$ u) <*> v"  applicativeApplyRight
        it "Apply Left:  u <* v ≡ liftA2 const u v" applicativeApplyLeft
        it "(<*>) via liftA2: (<*>) ≡ liftA2 id"
            applicativeApViaLiftA2
        it "liftA2 via (<*>): liftA2 f x y ≡ f <$> x <*> y"
            applicativeLiftA2ViaAp
    describe "Alternative instance for Trial" $ do
        it "Associativity: a <|> (b <|> c) ≡ (a <|> b) <|> c"
            alternativeAssociativity
        it "Right Identity: x <|> empty ≡ x" alternativeRightIdentity
        it "Left Identity:  empty <|> x ≡ x" alternativeLeftIdentity
    describe "Monad instance for Trial" $ do
        it "Left Identity: return a >>= k ≡ k a" monadLeftIdentity
        it "Right Identity: m >>= return ≡ m" monadRightIdentity
        it "Associativity: m >>= (\\x -> k x >>= h) ≡ (m >>= k) >>= h"
            monadAssociativity
        it "Monad and Applicative: m1 <*> m2 ≡ m1 >>= (x1 -> m2 >>= (x2 -> return (x1 x2)))"
            monadApplicative

----------------------------------------------------------------------------
-- Semigroup instance properties
----------------------------------------------------------------------------

semigroupAssociativity :: Property
semigroupAssociativity = checkAssotiativityFor (genTrial genInt) (<>)

semigroupConcatenation :: Property
semigroupConcatenation = do
    let gen = genTrial genInt
    a <- forAll gen
    as <- forAll $ genSmallList gen
    let ne = a :| as
    sconcat ne === foldr1 (<>) ne

semigroupTimes :: Property
semigroupTimes = do
    a <- forAll $ genTrial genInt
    n <- forAll genSmallInt
    stimes n a === foldr1 (<>) (replicate n a)

----------------------------------------------------------------------------
-- Functor instance laws
----------------------------------------------------------------------------

functorIdentity :: Property
functorIdentity = hedgehog $ do
    a <- forAll $ genTrial genInt
    fmap id a === id a

functorComposition :: Property
functorComposition = hedgehog $ do
    a <- forAll $ genTrial genInt
    f <- forAllWith (const "f") genFunction
    g <- forAllWith (const "g") genFunction
    fmap f (fmap g a) === fmap (f . g) a

functorConst :: Property
functorConst = hedgehog $ do
    a <- forAll $ genTrial genInt
    let x = 'X'
    fmap (const x) a === (x <$ a)

----------------------------------------------------------------------------
-- Applicative instance properties
----------------------------------------------------------------------------

functorApplicative :: Property
functorApplicative = hedgehog $ do
    f <- forAllWith (const "f") genFunction
    x <- forAll $ genTrial genInt
    fmap f x === (pure f <*> x)

applicativeIdentity :: Property
applicativeIdentity = hedgehog $ do
    vx <- forAll $ genTrial genInt
    (pure id <*> vx) === vx

applicativeComposition :: Property
applicativeComposition = hedgehog $ do
    vf <- forAllWith (const "f") $ genTrial genFunction
    vg <- forAllWith (const "g") $ genTrial genFunction
    vx <- forAll $ genTrial genInt
    (pure (.) <*> vf <*> vg <*> vx) === (vf <*> (vg <*> vx))

applicativeHomomorphism :: Property
applicativeHomomorphism = hedgehog $ do
    f <- forAllWith (const "f") genFunction
    x <- forAll genInt
    (pure f <*> pure x) === pure @(Trial Int) (f x)

applicativeInterchange :: Property
applicativeInterchange = hedgehog $ do
    vf <- forAllWith (const "f") $ genTrial genFunction
    x <- forAll genInt
    (vf <*> pure x) === (pure ($ x) <*> vf)

applicativeApplyRight :: Property
applicativeApplyRight = hedgehog $ do
    let genVal = genTrial genInt
    vy <- forAll genVal
    vx <- forAll genVal
    (vy *> vx) === ((id <$ vy) <*> vx)

applicativeApplyLeft :: Property
applicativeApplyLeft = hedgehog $ do
    let genVal = genTrial genInt
    vy <- forAll genVal
    vx <- forAll genVal
    (vy <* vx) === liftA2 const vy vx

applicativeApViaLiftA2 :: Property
applicativeApViaLiftA2 = hedgehog $ do
    vf <- forAllWith (const "f") $ genTrial genFunction
    vx <- forAll $ genTrial genInt
    (vf <*> vx) === liftA2 id vf vx

applicativeLiftA2ViaAp :: Property
applicativeLiftA2ViaAp = hedgehog $ do
    f <- forAllWith (const "f") genFunction2
    vx <- forAll $ genTrial genInt
    vy <- forAll $ genTrial genInt
    liftA2 f vx vy === (f <$> vx <*> vy)

----------------------------------------------------------------------------
-- Alternative instance properties
----------------------------------------------------------------------------

alternativeAssociativity :: Property
alternativeAssociativity = checkAssotiativityFor (genTrial genInt) (<|>)

alternativeRightIdentity :: Property
alternativeRightIdentity = hedgehog $ do
    x <- forAll $ genTrial genSmallInt
    (x <|> empty) === x

alternativeLeftIdentity :: Property
alternativeLeftIdentity = hedgehog $ do
    x <- forAll $ genTrial genInt
    (empty <|> x) === x

----------------------------------------------------------------------------
-- Monad instance properties
----------------------------------------------------------------------------

monadLeftIdentity :: Property
monadLeftIdentity = hedgehog $ do
    a <- forAll genInt
    k <- forAllWith (const "k") genBindFunction
    (return a >>= k) === k a

monadRightIdentity :: Property
monadRightIdentity = hedgehog $ do
    x <- forAll $ genTrial genInt
    (x >>= return) === x

monadAssociativity :: Property
monadAssociativity = hedgehog $ do
    m <- forAll $ genTrial genInt
    k <- forAllWith (const "k") genBindFunction
    h <- forAllWith (const "h") genBindFunction
    (m >>= (\x -> k x >>= h)) === ((m >>= k) >>= h)

monadApplicative :: Property
monadApplicative = hedgehog $ do
    mf <- forAllWith (const "mf") $ genTrial genFunction
    t <- forAll $ genTrial genInt
    (mf <*> t) === (mf >>= (\f -> t >>= (\a -> return (f a))))

----------------------------------------------------------------------------
-- Property helpers
----------------------------------------------------------------------------

{- | Property test for the associativity law:

@
a ⊗ (b ⊗ c) ≡ (a ⊗ b) ⊗ c
@
-}
checkAssotiativityFor
    :: (Show a, Eq a)
    => Gen a
    -> (a -> a -> a)
    -> Property
checkAssotiativityFor gen op = hedgehog $ do
    a <- forAll gen
    b <- forAll gen
    c <- forAll gen
    a `op` (b `op` c) === (a `op` b) `op` c
