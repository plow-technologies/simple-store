[![Circle CI](https://circleci.com/gh/plow-technologies/simple-store.svg?style=shield)](https://circleci.com/gh/plow-technologies/simple-store)
# simple-store

Often in haskell you have a shared data type that needs persistence, atomicity and consistency.


## Usage

The core datatype for SimpleStore is

``` haskell

data SimpleStore st = SimpleStore {
    storeFP     :: FilePath
  , storeState  :: TVar st
  , storeLock   :: TMVar StoreLock
  , storeHandle :: Maybe Handle
}


```

Say your Datatype is :

``` haskell

data Address = Address { street :: Street , town::Town}
  deriving (Generic,Eq,Ord)

instance Serialize Address where 


``` haskell


You might write:

``` haskell
-- | makeSimpleStore :: FilePath -> st -> IO (Either StoreError (SimpleStore st))

djangosAddress = do
   eSt(makeSimpleStore "DjangoAddress" (Address MinorSwingBlvd GypsyTown))
   either (\e -> fail (show e) ) (return st)  eSt


```
A directory containing a serialized version of Django's address is saved for you
**Note** you can use text serialization too so it is readable.


Now you can share that between processes (in a Yesod for instance)



lets walk there
``` haskell


walkToDjangosHouse :: IO () 
walkToDjangosHouse = do
  djAddr <- getSimpleStore djangosAddress
  walkToDjangosHouse djAddr 

```

