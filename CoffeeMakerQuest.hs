module CoffeeMakerQuest where
import Data.List

data Items = Items { coffee :: Bool
                   , cream  :: Bool
                   , sugar  :: Bool }
                   deriving (Eq, Show)

noItems    = Items False False False
justCoffee = Items True  False False
justCream  = Items False True  False
justSugar  = Items False False True
allItems   = Items True  True  True

data Room = Room { roomDesc  :: String
                 , furnDesc  :: String
                 , roomItems :: Items }
                 deriving (Eq, Show)

theRooms =
    [ Room "ornate hallway" "robust plant"   justCoffee
    , Room "furry bedroom"  "groovy beanbag" noItems
    , Room "slimy bathroom" "wet towel"      justCream
    , Room "pink kitchen"   "shiny toaster"  justSugar ]

data GameState = GameState { gameMessage :: String
                           , currItems   :: Items
                           , currRoom    :: Int }
               | WinState
               | LoseState
               deriving (Eq, Show)

-- The initial game state
initState :: GameState
initState = GameState
    "CoffeeMakerQuest 3.0\nEnter N, S, L, D, I, or H." noItems 0

-- Describe the given room
roomMessage :: Room -> String
roomMessage room = "You are in a " ++ (roomDesc room) ++ "."

-- Describe the furniture in the given room
furnMessage :: Room -> String
furnMessage room = "You see a " ++ (furnDesc room) ++ "."

-- Describe what's in the given items
itemMessage :: Items -> String
itemMessage items = 
    if coffee items
    then if cream items
        then if sugar items
            then "coffee, cream, and sugar"
            else "coffee and cream"
        else if sugar items
            then "coffee and sugar"
            else "coffee"
    else
        if cream items
        then if sugar items
            then "cream and sugar"
            else "cream"
        else if sugar items
            then "sugar"
            else "nothing"

getItems :: Items -> Items -> Items
getItems fromItems toItems = Items
    ((coffee fromItems) || (coffee toItems))
    ((cream fromItems)  || (cream toItems))
    ((sugar fromItems)  || (sugar toItems))

moveNorth :: GameState -> GameState
moveNorth (GameState message currItems currRoom)
    | currRoom < length theRooms - 1
        = GameState "You walk through the north door." currItems (currRoom + 1)
    | otherwise
        = GameState "The room has no north door." currItems currRoom

moveSouth :: GameState -> GameState
moveSouth (GameState message items room)
    | room > 0  = GameState "You walk through the south door." items (room-1)
    | otherwise = GameState "The room has no south door." items room

lookAround :: GameState -> GameState
lookAround (GameState message currItems currRoom) =
    GameState ("You found " ++ (itemMessage rmItems) ++ ".")
        (getItems rmItems currItems) currRoom
        where rmItems = (roomItems (theRooms!!currRoom))

drinkItems :: GameState -> GameState
drinkItems (GameState _ items _)
    | items == allItems = WinState
    | otherwise         = LoseState

checkItems :: GameState -> GameState
checkItems (GameState message currItems currRoom) =
    GameState ("You have " ++ (itemMessage currItems) ++ ".")
        currItems currRoom

showHelp :: GameState -> GameState
showHelp (GameState message currItems currRoom) =
    GameState (intercalate "\n" 
        [ "n, N\tMove north"
        , "s, S\tMove south"
        , "l, L\tLook around"
        , "i, I\tCheck items"
        , "d, D\tDrink coffee"
        , "h, H\tShow help" ])
        currItems currRoom

badCommand :: GameState -> GameState
badCommand (GameState message currItems currRoom)
    = GameState "Invalid command." currItems currRoom

executeCommand :: String -> GameState -> GameState
executeCommand command
    | command `elem` ["n","N"] = moveNorth
    | command `elem` ["s","S"] = moveSouth
    | command `elem` ["l","L"] = lookAround
    | command `elem` ["d","D"] = drinkItems
    | command `elem` ["i","I"] = checkItems
    | command `elem` ["h","H"] = showHelp
    | otherwise                = badCommand

runLoop :: GameState -> IO ()
runLoop currState = case currState of
    WinState  -> putStrLn "You make a cup of coffee and drink it. You win!!!"
    LoseState -> putStrLn "You don't have all of the ingredients. You lose..."
    _         -> putStrLn (gameMessage currState)
              >> putStrLn "______________________________"
              >> putStrLn (roomMessage thisRoom)
              >> putStrLn (furnMessage thisRoom)
              >> getLine >>= runLoop . (`executeCommand` currState)
                 where thisRoom = theRooms!!(currRoom currState)

main :: IO ()
main = runLoop initState
