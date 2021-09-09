{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BangPatterns #-}
module Main where

import Options.Applicative
import System.Directory
import Data.List (isPrefixOf)
import System.IO


data GoalCmd
  = NewGoal String String Int
  | IncGoal String
  | DecGoal String
  | SetGoal String Int
  | ResizeGoal String Int
  | DeleteGoal String
  deriving Show

goal :: Parser String
goal = argument str $ mconcat
  [ metavar "GOAL"
  , completer $ mkCompleter $ \start -> do
      files <- getDirectoryContents goalDir
      pure $ filter (isPrefixOf start) files
  ]

size :: Parser Int
size = option auto $ mconcat
  [ long "size"
  , short 's'
  , metavar "SIZE"
  ]

progress :: Parser Int
progress = option auto $ mconcat
  [ long "progress"
  , short 'p'
  , metavar "PROGRESS"
  ]


blah :: Parser GoalCmd
blah = subparser $ mconcat
  [ command "new"
      $ flip info (progDesc "Create a new goal")
      $ NewGoal
          <$> goal
          <*> option str (long "name" <> short 'n' <> metavar "NAME")
          <*> size
  , command "inc"
      $ flip info (progDesc "Increment a goal")
      $ IncGoal <$> goal
  , command "dec"
      $ flip info (progDesc "Decrement a goal")
      $ DecGoal <$> goal
  , command "set"
      $ flip info (progDesc "Set a goal's progress")
      $ SetGoal <$> goal <*> progress
  , command "resize"
      $ flip info (progDesc "Resize a goal's progress")
      $ ResizeGoal <$> goal <*> size
  , command "delete"
      $ flip info (progDesc "Finish a goal")
      $ DeleteGoal <$> goal
  ]

goalDir :: FilePath
goalDir = "/home/sandy/.goals/"

goalPath :: String -> FilePath
goalPath = mappend goalDir


createGoal :: String -> String -> Int -> IO ()
createGoal goal name size =
  writeFile (goalPath goal) $ unlines
    [ name
    , show size
    , "0"
    ]

deleteGoal :: String -> IO ()
deleteGoal = removeFile . goalPath


modifyGoal :: (Int -> Int) -> (Int -> Int) -> String -> IO ()
modifyGoal mod_size mod_prog goal = do
  let file = goalPath goal

  h <- openFile file ReadWriteMode
  ls <- fmap lines $ hGetContents h

  -- Extremely high quality parser
  let !name = ls !! 0
      !size = read @Int $ ls !! 1
      !prog = read @Int $ ls !! 2

  hClose h

  writeFile file $ unlines
    [ name
    , show $ mod_size size
    , show $ mod_prog prog
    ]


main :: IO ()
main = do
  cmd <- execParser $ info (helper <*> blah) $ mconcat
    [ fullDesc
    , header "goals - a manager for conky-goals"
    ]
  case cmd of
    NewGoal g name n -> createGoal g name n
    IncGoal g        -> modifyGoal id (+ 1) g
    DecGoal g        -> modifyGoal id (subtract 1) g
    SetGoal g n      -> modifyGoal id (const n) g
    ResizeGoal g n   -> modifyGoal (const n) id g
    DeleteGoal g     -> deleteGoal g

