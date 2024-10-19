-- Define the module
module Logic
  ( Location,
    toLocation,
    fromLocation,
    feedback,
    GameState,
    initialGuess,
    nextGuess,
  )
where

-- Import modules
import Data.List (group, minimumBy, sort, (\\))
import Data.Ord (comparing)

-- Define data types
data Column = A | B | C | D | E | F | G | H
  deriving (Show, Eq, Ord, Enum, Bounded)

data Row = One | Two | Three | Four deriving (Eq, Ord, Enum, Bounded)

data Location = Coordinate Column Row deriving (Eq, Ord)

-- Define instances
instance Show Row where
  show One = "1"
  show Two = "2"
  show Three = "3"
  show Four = "4"

instance Show Location where
  show :: Location -> String
  show (Coordinate x y) = show x ++ show y

-- Define type aliases
type Guess = [Location]

type GameState = [Guess]

-- Convert a string to a location
toLocation :: String -> Maybe Location
toLocation [x, y] =
  if x `elem` ['A' .. 'H'] && y `elem` ['1' .. '4']
    then
      Just
        ( Coordinate
            (toEnum (fromEnum x - fromEnum 'A'))
            (toEnum (fromEnum y - fromEnum '1'))
        )
    else Nothing
toLocation _ = Nothing

-- Convert a location to a string
fromLocation :: Location -> String
fromLocation (Coordinate x y) = show x ++ show y

-- Calculate the number of targets that are at distances of 0, 1, and 2 from
--   the guesses
feedback :: [Location] -> [Location] -> (Int, Int, Int)
feedback targets guesses = (d0, d1, d2)
  where
    gDist = [minimum [calcDistance g t | t <- targets] | g <- guesses]
    d0 = length $ filter (== 0) gDist
    d1 = length $ filter (== 1) gDist
    d2 = length $ filter (== 2) gDist

-- Calculate the maximum Manhattan distance between two locations
calcDistance :: Location -> Location -> Int
calcDistance (Coordinate x1 y1) (Coordinate x2 y2) = max dx dy
  where
    dx = abs $ fromEnum x1 - fromEnum x2
    dy = abs $ fromEnum y1 - fromEnum y2

-- Generate the initial guess and game state
initialGuess :: ([Location], GameState)
initialGuess = (initGuess, initGameState)
  where
    initGuess = [Coordinate A One, Coordinate A Four, Coordinate H One]
    initGameState = allPossibleGuesses \\ [initGuess]

-- Generates a list of all possible locations on the board
allCoordinates :: [Location]
allCoordinates = [Coordinate x y | x <- [A .. H], y <- [One .. Four]]

-- Generates all possible guesses
allPossibleGuesses :: [Guess]
allPossibleGuesses =
  [ [c1, c2, c3]
    | c1 <- allCoordinates,
      c2 <- allCoordinates,
      c1 < c2,
      c3 <- allCoordinates,
      c2 < c3
  ]

-- Generate the next guess and game state based on the previous guess and
--   feedback
nextGuess :: ([Location], GameState) -> (Int, Int, Int) -> (Guess, GameState)
nextGuess (prevGuess, prevGameState) feedback = (curGuess, curGameState)
  where
    curGameState = filterGameState prevGameState feedback prevGuess
    curGuess = bestGuess curGameState

-- Filter the game state by removing guesses that do not match the feedback
filterGameState :: GameState -> (Int, Int, Int) -> [Location] -> GameState
filterGameState gameState (d0, d1, d2) guess =
  filter (\g -> feedback g guess == (d0, d1, d2)) gameState

-- Select the best guess based on the expected value of the feedback
bestGuess :: GameState -> Guess
bestGuess gameState = fst $ minimumBy (comparing snd) guessesWithEV
  where
    guessesWithEV = [(guess, calcEV guess gameState) | guess <- gameState]

-- Calculate the expected value of the feedback for a guess
calcEV :: Guess -> GameState -> Double
calcEV guess state = sum [p * p | p <- probabilities]
  where
    feedbackResults = map (`feedback` guess) state
    feedbackCounts = map length $ group $ sort feedbackResults
    totalFeedback = fromIntegral (length feedbackResults)
    probabilities = map ((/ totalFeedback) . fromIntegral) feedbackCounts
