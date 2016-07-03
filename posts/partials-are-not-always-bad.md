---
title: Partials are not always bad
author: Mark Karpov
description: Some thoughts against widespread hatred towards partial functions
published: July 3, 2016
---

From time to time you may see people who react very negatively to any use of
partial functions (i.e. functions that are defined not for all input values
or diverge for some input values). At first sight it is understandable: if
you pass a “wrong” value in, your programs will blow up — we don't want this
(that's why we use Haskell after all). But programming is rarely pure and
sterile, programming is about making things (sometimes big, complex things)
work correctly, and for that we can use different tools as long as we know
what we are doing.

Avoidance of partial functions at any cost is like avoidance of exceptions
(or Template Haskell, whatever) — you already have them in language and
compiler, so why reject them?

## `fromJust`

Not long time ago I have seen a developer who hunted down all calls of
`fromJust` in a codebase. From his comments I understood that he thinks that
`fromJust` is not acceptable at all, not in any case — because it's partial.

I immediately remembered a case when I myself used `fromJust` actively
without feeling shameful about it at all. I worked with API that has a
number of “smart constructors” that produce `Maybe a` values — `Nothing`
signified unacceptable input. I needed to create quite a few constants in
code and I needed them to be of type `a`, not `Maybe a`. So I went ahead
with this pattern (this is made up for simplicity but original is quite
similar):

```haskell
-- | Create a number greater than 1 and lesser than 100. Return 'Nothing' in
-- case of invalid input.

mkIndex :: Int -> Maybe SureCorrectNum

myConst :: SureCorrectNum
myConst = fromJust (mkIndex 5)
```

There is example of similar thing in Megaparsec (two versions — safe and
unsafe, the unsafe one is used for constants all around the place):

```haskell
-- | Construction of 'Pos' from an instance of 'Integral'. The function
-- throws 'InvalidPosException' when given non-positive argument. Note that
-- the function is polymorphic with respect to 'MonadThrow' @m@, so you can
-- get result inside of 'Maybe', for example.
--
-- @since 5.0.0

mkPos :: (Integral a, MonadThrow m) => a -> m Pos
mkPos x =
  if x < 1
    then throwM InvalidPosException
    else (return . Pos . fromIntegral) x
{-# INLINE mkPos #-}

-- | Dangerous construction of 'Pos'. Use when you know for sure that
-- argument is positive.
--
-- @since 5.0.0

unsafePos :: Word -> Pos
unsafePos x =
  if x < 1
    then error "Text.Megaparsec.Pos.unsafePos"
    else Pos x
{-# INLINE unsafePos #-}

-- | Construct initial position (line 1, column 1) given name of source
-- file.

initialPos :: String -> SourcePos
initialPos n = SourcePos n u u
  where u = unsafePos 1
{-# INLINE initialPos #-}
```

Most of the time the types should lead you and `fromJust` and `head` are
dangerous indeed, but there are cases (and more than may seem) where they
are quite OK and convenient solution.

## `undefined`

`undefined` seems to be a really bad kid. It always diverges. Should we
never it as well?

Before `Data.Proxy` came in use, we could pass type information using
`undefined` like so:

```haskell
myFunction (undefined :: MyType) x y z
```

And honestly it is OK as long as that value is not valuated. Should we ever
leave `undefined` in production code nowadays?

I have been writing some Hspec tests recently.
[The project](https://github.com/stackbuilders/stache) is actually open
source, although it's not yet released. The code loads Mustache specs in
YAML format and makes Hspec expectations from them:

```haskell
specData :: String -> ByteString -> Spec
specData aspect bytes = describe aspect $ do
  let handleError = expectationFailure . parseErrorPretty
  case decodeEither' bytes of
    Left err ->
      it "should load YAML specs first" $
        expectationFailure (prettyPrintParseException err)
    Right SpecFile {..} ->
      forM_ specTests $ \Test {..} ->
        it (testName ++ ": " ++ testDesc) $
          case compileMustacheText (PName $ T.pack testName) testTemplate of
            Left perr -> handleError perr
            Right Template {..} -> do
              ps1 <- forM (M.keys testPartials) $ \k -> do
                let pname = PName k
                case parseMustache (T.unpack k) (testPartials ! k) of
                  Left perr -> handleError perr >> undefined -- HERE
                  Right ns  -> return (pname, ns)
              let ps2 = M.fromList ps1 `M.union` templateCache
              renderMustache (Template templateActual ps2) testData
                `shouldBe` testExpected
```

Note the situation on 5th line from the end. We need to return a tuple here,
but I want the test to fail with `expectationFailure` using pretty-printed
error message if my parser chokes on any partial from test data.
`expectationFailure` has type `String -> Expectation` (sans implicit
parameters). Now:

```haskell
type Expectation = Assertion -- from Hspec source
type Assertion   = IO ()     -- from HUnit source
```

That's it. Even though the code without `undefined` makes sense, I cannot
return things of different types from different clauses of `case`. So I
added here `undefined` to make it type check without changing semantics at
all. Have it made the world worse? I don't think so.

## Representing non-negative numbers

I always disliked the use of `Int` everywhere to represent things that
cannot actually be negative: lengths, sizes, indicies… Let's make the
decision to replace `Int` with another type already present in modern
`base`. What shall it be?

* `Word` — has only non-negative values. Operations on `Word` are not
  partial (unless you divide by zero, but `Int`s are no better in this
  respect).

* `Natural` — a newer type, if an operation on `Natural`s produces negative
  value, you get an error, your programs blows.

What do we use, “partial” `Natural` or (almost) “total” `Word`? GHCi to the
rescue:

```haskell
λ> (5 :: Natural) - 6
*** Exception: arithmetic underflow
λ> (5 :: Word) - 6
18446744073709551615 -- welcome to C!
```

Personally, I like the first behavior more. `Natural` by far is the most
logical trade-off:

* It's instance of various type classes that allow to work with it as with a
  normal number.

* If things go wrong you know about it.

Don't let prejudices against partial functions fool you, use `Natural`.

## Conclusion

Tools are just tools, and partial functions sure have their applications in
functional programming. As long as you know what you are doing, use them.
