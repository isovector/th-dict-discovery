# th-dict-discovery

`th-dict-discovery` is a library to get your hands on the in-scope, concrete
instances of a class. The motivating use-case is to generate property tests to
automatically prove new instances of a class follow the laws.

This library uses Template Haskell to provide a list of existentialized `Dict`s
for each concrete (entirely monomorphic) instance in scope.

```haskell
defInstances :: [SomeDict1 Default]
defInstances = $(someDicts ''Default)
```

These lists can be consumed via
`withSomeDict1 :: (forall a. c a => Proxy a -> r) -> SomeDict1 c -> r`, for
example, we can generate `hspec` tests:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

spec :: Spec
spec = describe "each default instance" .
  forM_ defInstances $ withSomeDict1 $ \(_ :: Proxy a) ->
    it "shouldn't be undefined" $
      seq (def @a) True `shouldBe` True
```

The library also defines up to `SomeDict8`, with eliminators up to
`withSomeDict8`. These correspond to multi-parameter typeclasses with
first-order, potentially polykinded parameters.

The TH function `someDicts` is smart enough to generate the correct arity of
`SomeDictN` for the class under inspection, but at this time, you're responsible
for using the correct `withSomeDictN` to consume it.

