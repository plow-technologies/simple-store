# Changelog

# Last Updated 5-14-18

Simple-Store
+ 4.1.0
  upgrade to ghc 8.2.2, lts-11.7
+ 4.0.0
  Export STM versions of many functions
+ 3.1.0
  use atomic-write to handle file writing
+ 3.0.1
  add modifySimpleStoreResultWith
+ 3.0.0
  removed close store  
+ 2.1.0
  removed Handle holding
+ 2.0.4
  open.lock functionality removed
  made fsync run inbetweeen var locks to force it better
	
+ 2.0.3
  fsync on checkpoints and locks
  createCheckpointImmediate allows fsync to be called optionally.
  immediate checkpoints always on lock files
	
+ 2.0.2
   better exception handling in opening state store.
   Separated out different try/catch places
+ 2.0.1.1
   added a new exception "NoStoreFileFound"
+ 2.0.1
   last touch not present shouldn't throw exception
+ 2.0.0
   simple-store now uses a last touch file to determine the correct file to decode
+ 1.2.0
   stop deleting, only rewriting
+ 1.1.1
   strictness annotation on checkpoint creation
+ 1.1.0
   removeTree fp, removed!
+ 1.0.0
	We went down an odd path in 0.3.0 - 0.2.0
	This restores order
	
+ 0.1.4
  Better filename creation routine
  More strictness in the base types
  MVar in lock instead of TMVar for better sleep/awake performance

+ 0.1.3
  Attempt to Open
+ 0.1.2
  added 'attemptToOpen' function for initialization of pre-created states in an
  enviornment where they might need to be re-initialized.

