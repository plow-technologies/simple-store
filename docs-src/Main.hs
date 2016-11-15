{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Development.Shake ( shakeArgs
                         , shakeOptions
                         , shakeFiles
                         , shakeProgress
                         , command_
                         , Rules
                         , Action
                         , progressSimple
                         , want
                         , need
                         , (%>)
                         , liftIO)
import           Development.Shake.FilePath ((</>))
import           Control.Monad (filterM)

import qualified Filesystem
--       (isDirectory, getModified, listDirectory, isFile, writeFile) 
import qualified Filesystem.Path.CurrentOS as CurrentOS
--       (FilePath, (</>),  fromText)
import Data.Either (rights)
import Data.Text (Text,unpack)
import Control.Lens
import Data.Aeson.Lens
import Text.Regex.Lens
import Text.Regex.Quote
import Text.Regex.Posix (Regex)
import qualified Data.Yaml as Yaml


--------------------------------------------------
-- Types
--------------------------------------------------


data GeneratedStaticRules = GeneratedStaticRules {
                                  generatedWants :: [FilePath],
                                  generatedRules :: [Rules ()] }



data NamesThatMustBeDiscovered = NamesThatMustBeDiscovered
  {
    cabalPath :: FilePath,
      ltsPath :: FilePath,
  packageName :: String
  } deriving (Eq,Show,Ord)







main :: IO ()
main = shakeArgs shakeOptions { shakeFiles    = buildDir
                              , shakeProgress = progressSimple}
                 buildTheDocsRules
  where
    buildDir = "_docBuild"



--------------------------------------------------
-- Rules
--------------------------------------------------
-- | Top level of document generation
buildTheDocsRules :: Rules ()
buildTheDocsRules = do
  GeneratedStaticRules wants rules <- runDynamics
  want wants
  _ <- sequence rules
  return ()




-- | build elements that depend on specific configurations of stack.yaml
--  this mostly involves setting up the correct cabal and lts directories
--  for copying over the documentation. 
runDynamics = do
  (Just names) <- liftIO buildNamesThatMustBeDiscovered
  return $ GeneratedStaticRules [haddockInDocsIndex     , haddockInStackWorkIndex names]
                                [stackHaddockRule names , docsHaddockRule names]




-- | Build the documentation in the .stack-work folder  

stackHaddockRule :: NamesThatMustBeDiscovered -> Rules ()
stackHaddockRule names = haddockInStackWorkIndex names %> \out -> do
  liftIO $ putStrLn "Poop"
  liftIO $ print out
  stackHaddockCommand


-- | Copy the documentation into the destination folder 
-- docsHaddockRule :: Rules ()
docsHaddockRule names = haddockInDocsIndex %> \_ -> do
    need [haddockInStackWorkIndex names]    
    copyOtherPackagesCommand names -- This needs to come before copyHaddock
    copyHaddockCommand names












--------------------------------------------------
-- Commands and other Action
--------------------------------------------------

stackHaddockCommand :: Action ()
stackHaddockCommand = command_ [] cmdString opts
  where
    cmdString =  "stack"
    opts      = ["haddock"]
    

copyHaddockCommand :: NamesThatMustBeDiscovered -> Action ()
copyHaddockCommand names = command_ [] "rsync" ["-arv",haddockInStackWork names  </>"." , haddockInDocs  ]

copyOtherPackagesCommand :: NamesThatMustBeDiscovered -> Action ()
copyOtherPackagesCommand names = command_ [] "rsync" ["-arv", haddockOtherPackagesInStackWork names </> ".", haddockInDocs]







-------------------------------------------------
-- Declarations for various directories
-------------------------------------------------

-- Hidden directory for generated documents
haddockInStackWork :: NamesThatMustBeDiscovered -> FilePath
haddockInStackWork names = ".stack-work" </> "dist" </>"x86_64-linux"</>cabalPath</>"doc"</> "html" </> packageName
  where
    (NamesThatMustBeDiscovered { cabalPath
                               , ltsPath
                               , packageName}) = names

-- index.html for package docs
haddockInStackWorkIndex :: NamesThatMustBeDiscovered -> FilePath
haddockInStackWorkIndex names = haddockInStackWork names </> "index.html"

haddockOtherPackagesInStackWork :: NamesThatMustBeDiscovered -> FilePath
haddockOtherPackagesInStackWork names = ".stack-work"</>"install"</>"x86_64-linux"</>"lts-6.13"</>"7.10.3"</>"doc"
  where
    (NamesThatMustBeDiscovered { cabalPath
                               , ltsPath
                               , packageName}) = names

haddockInDocs :: FilePath
haddockInDocs = "docs" 

haddockInDocsIndex :: FilePath
haddockInDocsIndex = haddockInDocs </> "index.html"


stackWorkInstallPath :: CurrentOS.FilePath -> CurrentOS.FilePath
stackWorkInstallPath wd = (wd CurrentOS.</> ".stack-work" CurrentOS.</> "install")

--------------------------------------------------
-- Dynamic Directory Lookup
--------------------------------------------------


getDirectories :: CurrentOS.FilePath -> IO [CurrentOS.FilePath]
getDirectories wd = do
  dirs <- Filesystem.listDirectory wd
  filterM Filesystem.isDirectory dirs


-- Get a full path target
-- FilePath "<working-dir>/.stack-work/install/x86_64-linux"
getTarget :: IO CurrentOS.FilePath
getTarget = do
 wd         <- Filesystem.getWorkingDirectory
 (dir:_)    <- getDirectories $ stackWorkInstallPath wd
 return dir



buildNamesThatMustBeDiscovered = do
                                target       <- getTarget
                                eitherDirs   <- getDirectories target
                                mPkgName      <- getPackageInfo
                                let eitherTextDirs   = fmap unpack . CurrentOS.toText . CurrentOS.basename <$> eitherDirs 
                                    maybeLtsString   = eitherTextDirs ^? folded . _Right . regex [r|lts.*|] . matchedString
                                    maybeCabalString = eitherTextDirs ^? folded . _Right . regex [r|Cabal.*|] . matchedString
                                                                                                 
                                return $ NamesThatMustBeDiscovered <$> maybeLtsString  <*> maybeCabalString <*> mPkgName

ex = "test" ^? regex [r|te|] . matchedString :: Maybe String



--------------------------------------------------
-- Package Info
--------------------------------------------------

getPackageInfo :: IO (Maybe String)
getPackageInfo = do
  pkg <- Yaml.decodeFileEither "package.yaml" :: IO (Either Yaml.ParseException Yaml.Value)
  return $ (pkg ^? _Right . key "name" . _String <&> unpack)
