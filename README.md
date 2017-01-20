## haskell vs javascript

1. Create a project!
  1. Make sure stack is installed: `curl -sSL https://get.haskellstack.org/ | sh`
  1. Create new project `stack new helloworld new-template`
  1. Run setup to make sure the compiler is good to go `stack setup`
  1. Run build to compile `stack build`
  1. Run an executable to make sure everything worked as expected `stack exec helloworld-exe`

1. First challenge: reproducing data structure.
   - try and load json?
   - try and just reproduce it in haskell with records?
   - I already had the records and I was able to get Aeson working without toooo much difficulty

  1. In the first mention of `library` in the `helloworld.cabal` file, add the dependencies:
```
library
  hs-source-dirs:      src
  exposed-modules:     Lib
  build-depends:       base >= 4.7 && < 5
                     , aeson
                     , bytestring
  default-language:    Haskell2010
```
  1. aeson is used to encode and decode JSON
  1. bytestring is used to get the file contents into haskell
  1. Add a language extension pragma to let us have Haskell implement the encoding patterns from json to haskell and back {-# LANGUAGE DeriveGeneric #-}
  1. Add our imports
```
import Data.Aeson (eitherDecode, FromJSON)
import GHC.Generics
import Data.ByteString.Lazy as L
```
    1. `FromJSON` is a Typeclass that the type we want to decode needs to implement an instance of in order for haskell to decode json strings to that type
    1. `eitherDecode` has a type of `eitherDecode :: FromJSON a => ByteString -> Either String a` so it will take a ByteString and try to return a String representation of a type that implements FromJSON
    1. `Generics` lets haskell derive an instance of FromJSON from our type without us having to do the busy work
    1. `ByteString` lets us read data from a file

  1. Create our type
```
data Person
  = Person { name   :: String
           , sex    :: String
           , born   :: Int
           , died   :: Int
           , father :: String
           , mother :: String
            } deriving (Show, Generic)

```
 - note you should probably use Text here, for reasons that people try to tell me that String is bad that I can't reexplain
 - if you do use text you should be able to use a language extension pragma to let you use it mainly as you would String, something like `{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}`

  1. Create an instance of FromJSON for our type `instance FromJSON Person`
  1. Get a reference to the file of JSON and use ByteStream to read it
```
jsonFile :: FilePath
jsonFile = "ancestry.json"

getJSON :: IO L.ByteString
getJSON = L.readFile jsonFile
```
  1. Use an IO Monad to print out the result of decoding the file contents
    - <$> is an alias for fmap, which is analogous to map except it takes in a type with an instance of the Functor typeclass
    - the result of the decode attempt is bound to v
    - if the decode is successfull it prints out the resulting string
    - if the decode fails it prints out an error string
```
ancestors :: IO ()
ancestors = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
  case d of
    Left err -> Prelude.putStrLn $ "error: " ++ err
    Right as -> print as

```
  1. demonstrate mapping to get name: `Right as -> print $ Prelude.map name as`
  1. demonstrate filtering to get bornAfter `Right as -> print $ Prelude.map (bornAfter 1850) as`

```
bornAfter :: Int -> Person -> Bool
bornAfter n p = born p > n
-- other code
Right as -> print $ Prelude.map name $ Prelude.filter (bornAfter 1850) as
```

  1. demonstrate fold1 with ancienter
    - foldl is like reduce (applies a accumulator function over a foldable left to right)
    - foldr is like reduce (applies a accumulator function over a foldable right to left)
    - foldl1 and foldr1 allow you to leave out the initial arg
```
ancienter :: Person -> Person -> Person
ancienter a p = if born a < born p then a else p
-- other code
Right as -> print $ Prelude.foldl1 ancienter as
```
  1.demonstrate composition with average, age, male and female
```
average :: [Int] -> Float
average xs =
  let x = fromIntegral $ P.foldl (+) 0 xs
      y = fromIntegral $ P.length xs
  in x / y

age :: Person -> Int
age p = died p - born p

male :: Person -> Bool
male p = "m" == sex p

female :: Person -> Bool
female p = "f" == sex p
--other code
Right as -> print $ average $ P.map age $ P.filter male as

```
