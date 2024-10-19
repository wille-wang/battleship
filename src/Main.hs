-- Define the module
module Main where

-- Import modules
import Logic
  ( GameState,
    Location,
    feedback,
    fromLocation,
    initialGuess,
    nextGuess,
    toLocation,
  )
import System.Exit

testCase = "F1 D2 G4"

-- Define the main function that initializes the game
main :: IO ()
main = do
  case mapM toLocation $ words testCase of
    Just targetLocations@[_, _, _] ->
      runGame targetLocations
    _ -> do
      putStrLn $
        "toLocation Failed to convert one of "
          ++ testCase
          ++ " to a Location"
      exitFailure

-- Start the game loop
runGame :: [Location] -> IO ()
runGame targetLocations = do
  putStrLn $ "Searching for target locations: " ++ displayLocations targetLocations
  let (initialGuessLocations, gameState) = initialGuess
  gameLoop targetLocations initialGuessLocations gameState 1

-- Loop until the correct guess is found
gameLoop :: [Location] -> [Location] -> Logic.GameState -> Int -> IO ()
gameLoop targetLocations currentGuess gameState guessCount = do
  putStrLn $ "Your guess #" ++ show guessCount ++ ":  " ++ displayLocations currentGuess
  let guessFeedback = feedback targetLocations currentGuess
  putStrLn $ "    Feedback:  " ++ show guessFeedback
  if guessFeedback == (3, 0, 0)
    then do
      putStrLn $ "You got it in " ++ show guessCount ++ " guesses!"
    else do
      let (nextGuessLocations, newGameState) = nextGuess (currentGuess, gameState) guessFeedback
      gameLoop targetLocations nextGuessLocations newGameState (guessCount + 1)

-- Convert a list of locations to a string
displayLocations :: [Location] -> String
displayLocations = unwords . (fromLocation <$>)
