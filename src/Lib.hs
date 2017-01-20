{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Aeson (eitherDecode, FromJSON)
import GHC.Generics
import qualified Data.ByteString.Lazy as L

data Person
  = Person { name   :: String
           , sex    :: String
           , born   :: Int
           , died   :: Int
           , father :: String
           , mother :: String
            } deriving (Show, Generic)

instance FromJSON Person

jsonFile :: FilePath
jsonFile = "ancestry.json"

getJSON :: IO L.ByteString
getJSON = L.readFile jsonFile

bornAfter :: Int -> Person -> Bool
bornAfter n p = born p > n

l = Person "Luke" "m" 1988 2500 "dad" "mama"
p = Person "Phil" "m" 1985 2200 "dad" "mama"

ancienter :: Person -> Person -> Person
ancienter a b = if born a < born b then a else b

byName :: String -> [Person] -> Person
byName n ps = head $ filter (\p -> name p == n) ps

average :: [Int] -> Float
average ns = x / y
  where x = fromIntegral $ sum ns
        y = fromIntegral $ length ns

age :: Person -> Int
age p = died p - born p

-- male :: Person -> Bool
-- male p = "m" == sex p
--
-- female :: Person -> Bool
-- female p = "f" == sex p
determineSex :: String -> Person -> Bool
determineSex s p = s == sex p

ancestors :: IO ()
ancestors = do
  d <- (eitherDecode <$> getJSON) :: IO (Either String [Person])
  case d of
    Left err -> putStrLn $ "error: " ++ err
    Right as -> print $ average $ map age $ filter (determineSex "f") as
    -- Right as -> print $ map name as
