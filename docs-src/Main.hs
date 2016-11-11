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
















main :: IO ()
main = shakeArgs shakeOptions { shakeFiles = buildDir
                              , shakeProgress = progressSimple}
                 buildTheDocsRules
  where
    buildDir = "_docBuild"



--------------------------------------------------
-- Rules
--------------------------------------------------

buildTheDocsRules :: Rules ()
buildTheDocsRules = do
  want [haddockInDocsIndex, haddockInStackWorkIndex]
  stackHaddockRule
  docsHaddockRule 




-- | Build the documentation in the .stack-work folder  
stackHaddockRule :: Rules ()
stackHaddockRule = haddockInStackWorkIndex %> \_ -> do
  stackHaddockCommand


-- | Copy the documentation into the destination folder 
docsHaddockRule :: Rules ()
docsHaddockRule = haddockInDocsIndex %> \_ -> do
    need [haddockInStackWorkIndex]
    copyOtherPackagesCommand -- This needs to come before copyHaddock
    copyHaddockCommand













--------------------------------------------------
-- Commands and other Action
--------------------------------------------------

stackHaddockCommand :: Action ()
stackHaddockCommand = command_ [] cmdString opts
  where
    cmdString =  "stack"
    opts      = ["haddock"]
    

copyHaddockCommand :: Action ()
copyHaddockCommand = command_ [] "rsync" ["-arv",haddockInStackWork </>"." , haddockInDocs  ]

copyOtherPackagesCommand :: Action ()
copyOtherPackagesCommand = command_ [] "rsync" ["-arv", haddockOtherPackagesInStackWork </> ".", haddockInDocs]







-------------------------------------------------
-- Declarations for various directories
-------------------------------------------------

-- Hidden directory for generated documents
haddockInStackWork :: FilePath
haddockInStackWork = ".stack-work" </> "dist" </>"x86_64-linux"</>"Cabal-1.22.5.0"</>"doc"</> "html" </> "simple-store"

-- index.html for package docs
haddockInStackWorkIndex = haddockInStackWork </> "index.html"

haddockOtherPackagesInStackWork = ".stack-work"</>"install"</>"x86_64-linux"</>"lts-6.13"</>"7.10.3"</>"doc"

haddockInDocs :: FilePath
haddockInDocs = "docs" 

haddockInDocsIndex :: FilePath
haddockInDocsIndex = haddockInDocs </> "index.html"

