{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Serialize
import GHC.Generics
import SimpleStore

data Address = Address 
  { street :: String
  , town   :: String
  } deriving (Eq,Ord,Generic,Show)

instance Serialize Address

main :: IO ()
main = do
  -- simpleStoreValue makeSimpleStore "file/path/to/store/value" valueYouWantToSerialize
  -- eSimpleStoreValue <- openSimpleStore "file/path/where/value/is/store" :: IO (Either StoreError (SimpleStore SerializedType))
  -- value <- getSimpleStore simpleStoreValue
  _djangoAddressSimpleStore <- makeSimpleStore "DjangoAddress" (Address "Minor Swing Blvd" "Gypsy Town")
  eDjangoAddressSimpleStore <- openSimpleStore "DjangoAddress" :: IO (Either StoreError (SimpleStore Address))
  case eDjangoAddressSimpleStore of
    Left _storeError -> print "storeError"
    Right djangoAddressSimpleStore -> do
      djangoAddress <- getSimpleStore djangoAddressSimpleStore
      print "Successfully retrieved DjangoAddress with simple-store"
      print djangoAddress