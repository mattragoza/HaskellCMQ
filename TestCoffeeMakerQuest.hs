{-# LANGUAGE TemplateHaskell  #-}
import qualified CoffeeMakerQuest as CMQ
import Test.QuickCheck
import Test.QuickCheck.All
import Data.List

n :: Int
n = length CMQ.theRooms

-- Generate arbitrary rooms from the room list
instance Arbitrary CMQ.Room where
    arbitrary = do
        room <- choose (0, n-1)
        return $ CMQ.theRooms!!room

-- Generate arbitrary sets of items
instance Arbitrary CMQ.Items where
    arbitrary = do
        coffee <- choose (True, False)
        cream  <- choose (True, False)
        sugar  <- choose (True, False)
        return $ CMQ.Items coffee cream sugar

-- Generate arbitrary internal game states
instance Arbitrary CMQ.GameState where
    arbitrary = do
        message <- arbitrary
        items   <- arbitrary
        room    <- choose (0, n-1)
        return $ CMQ.GameState message items room

-- Room message describes the room
prop_roomDescInRoomMessage :: CMQ.Room -> Bool
prop_roomDescInRoomMessage room =
    CMQ.roomDesc room `isInfixOf` CMQ.roomMessage room

-- Room descriptions are unique
prop_roomDescIsUnique :: CMQ.Room -> CMQ.Room -> Bool
prop_roomDescIsUnique room1 room2 =
    if room1 /= room2
    then CMQ.roomDesc room1 /= CMQ.roomDesc room2
    else True

-- Furniture message describes the room furniture
prop_furnDescInFurnMessage :: CMQ.Room -> Bool
prop_furnDescInFurnMessage room =
    CMQ.furnDesc room `isInfixOf` CMQ.furnMessage room

-- Furniture descriptions are unique
prop_furnDescIsUnique :: CMQ.Room -> CMQ.Room -> Bool
prop_furnDescIsUnique room1 room2 =
    if room1 /= room2
    then CMQ.furnDesc room1 /= CMQ.furnDesc room2
    else True

-- Coffee is in the item message iff it's in the items
prop_coffeeInItemMessage :: CMQ.Items -> Bool
prop_coffeeInItemMessage (CMQ.Items coffee x y) =
    coffee == "coffee" `isInfixOf` message
    where message = CMQ.itemMessage (CMQ.Items coffee x y)

-- Cream is in the item message iff it's in the items
prop_creamInItemMessage :: CMQ.Items -> Bool
prop_creamInItemMessage (CMQ.Items x cream y) =
    cream == "cream" `isInfixOf` message
    where message = CMQ.itemMessage (CMQ.Items x cream y)

-- Sugar is in the item message iff it's in the items
prop_sugarInItemMessage :: CMQ.Items -> Bool
prop_sugarInItemMessage (CMQ.Items x y sugar) =
    sugar == "sugar" `isInfixOf` message
    where message = CMQ.itemMessage (CMQ.Items x y sugar)

-- Getting the same items you already have 
-- results in having the same items as before
prop_getItemsIdentity :: CMQ.Items -> Bool
prop_getItemsIdentity items =
    items == CMQ.getItems items items

-- Getting the same items twice results in having 
-- the same items each time
prop_getItemsIdempotent :: CMQ.Items -> CMQ.Items -> Bool
prop_getItemsIdempotent items1 items2 =
    CMQ.getItems items1 items2 ==
    CMQ.getItems items1 (CMQ.getItems items1 items2)

-- Getting from no items results in having the
-- same items you had before
prop_getItemsFromNoItems :: CMQ.Items -> Bool
prop_getItemsFromNoItems items =
    items == CMQ.getItems CMQ.noItems items

-- Getting items when you have no items results
-- in having exactly the items you just got
prop_getItemsIntoNoItems :: CMQ.Items -> Bool
prop_getItemsIntoNoItems items =
    items == CMQ.getItems items CMQ.noItems

-- Getting all items results in having all items
prop_getItemsFromAllItems :: CMQ.Items -> Bool
prop_getItemsFromAllItems items =
    CMQ.allItems == CMQ.getItems CMQ.allItems items

-- Getting items when you already have all items
-- results in still having all items
prop_getItemsIntoAllItems :: CMQ.Items -> Bool
prop_getItemsIntoAllItems items =
    CMQ.allItems == CMQ.getItems items CMQ.allItems

-- Getting items A when you already have items B is the
-- same as getting items B when you already have items A
prop_getItemsCommutative :: CMQ.Items -> CMQ.Items -> Bool
prop_getItemsCommutative itemsA itemsB =
    CMQ.getItems itemsA itemsB ==
    CMQ.getItems itemsB itemsA  

-- Moving north when you are not in the northmost room
-- increments current room by 1, otherwise has no effect
prop_moveNorthOnce :: CMQ.GameState -> Bool
prop_moveNorthOnce state =
    if   CMQ.currRoom state < n-1
    then CMQ.currRoom nextState == CMQ.currRoom state + 1
    else CMQ.currRoom nextState == CMQ.currRoom state
    where nextState = CMQ.moveNorth state

-- Moving south when you are not in the southmost room
-- decrements the current room by 1, otherwise has no effect
prop_moveSouthOnce :: CMQ.GameState -> Bool
prop_moveSouthOnce state =
    if   CMQ.currRoom state > 0
    then CMQ.currRoom nextState == CMQ.currRoom state - 1
    else CMQ.currRoom nextState == CMQ.currRoom state
    where nextState = CMQ.moveSouth state

-- Moving north then south when you are not in the northmost
-- room has no effect, otherwise it decrements the room by 1
prop_moveNorthMoveSouth :: CMQ.GameState -> Bool
prop_moveNorthMoveSouth state =
    if   CMQ.currRoom state < n-1
    then CMQ.currRoom nextState == CMQ.currRoom state
    else CMQ.currRoom nextState == CMQ.currRoom state - 1
    where nextState = CMQ.moveSouth $ CMQ.moveNorth state

-- Moving south then north when you are not in the southmost
-- room has no effect, otherwise it increments the room by 1
prop_moveSouthMoveNorth :: CMQ.GameState -> Bool
prop_moveSouthMoveNorth state =
    if   CMQ.currRoom state > 0
    then CMQ.currRoom nextState == CMQ.currRoom state
    else CMQ.currRoom nextState == CMQ.currRoom state + 1
    where nextState = CMQ.moveNorth $ CMQ.moveSouth state

-- North is in the game message after moving north
prop_northInMoveNorthMessage :: CMQ.GameState -> Bool
prop_northInMoveNorthMessage state =
    "north" `isInfixOf` CMQ.gameMessage nextState
    where nextState = CMQ.moveNorth state

-- South is in the game message after moving south
prop_southInMoveSouthMessage :: CMQ.GameState -> Bool
prop_southInMoveSouthMessage state =
    "south" `isInfixOf` CMQ.gameMessage nextState
    where nextState = CMQ.moveSouth state

-- The items that were in the room are in the game message
-- after looking around the room
prop_roomItemsInlookAroundMessage :: CMQ.GameState -> Bool
prop_roomItemsInlookAroundMessage state =
    CMQ.itemMessage (CMQ.roomItems room) `isInfixOf`
    CMQ.gameMessage nextState
    where room = CMQ.theRooms!!(CMQ.currRoom state)
          nextState = CMQ.lookAround state

-- Looking around twice in a row results in same state
prop_lookAroundIdempotent :: CMQ.GameState -> Bool
prop_lookAroundIdempotent state =
    nextState1 == nextState2
    where nextState1 = CMQ.lookAround state
          nextState2 = CMQ.lookAround nextState1

-- Looking around gets the items that were in the room
prop_lookAroundGetsRoomItems :: CMQ.GameState -> Bool
prop_lookAroundGetsRoomItems state =
    CMQ.currItems nextState ==
    CMQ.getItems (CMQ.currItems state) (CMQ.roomItems room)
    where room = CMQ.theRooms!!(CMQ.currRoom state)
          nextState = CMQ.lookAround state

-- Looking around does not change what items are in the room
prop_lookAroundLeavesRoomItems :: CMQ.GameState -> Bool
prop_lookAroundLeavesRoomItems state =
    CMQ.roomItems after == CMQ.roomItems before
    where before = CMQ.theRooms!!(CMQ.currRoom state)
          after = CMQ.theRooms!!(CMQ.currRoom nextState)
          nextState = CMQ.lookAround state

-- The items in your inventory are in the game message
-- when you check your items
prop_currItemsInCheckItemsMessage :: CMQ.GameState -> Bool
prop_currItemsInCheckItemsMessage state =
    CMQ.itemMessage (CMQ.currItems state) `isInfixOf`
    CMQ.gameMessage nextState
    where nextState = CMQ.checkItems state

-- Checking items does not affect the current room or
-- current items
prop_checkItemsSameState :: CMQ.GameState -> Bool
prop_checkItemsSameState state =
    CMQ.currRoom  nextState == CMQ.currRoom  state &&
    CMQ.currItems nextState == CMQ.currItems state
    where nextState = CMQ.checkItems state

-- Drinking the items when you have all items wins the
-- game, otherwise you lose the game
prop_drinkItemsNeedAll :: CMQ.GameState -> Bool
prop_drinkItemsNeedAll state =
    if   CMQ.currItems state == CMQ.allItems
    then nextState == CMQ.WinState
    else nextState == CMQ.LoseState
    where nextState = CMQ.drinkItems state

-- Executing an invalid command does not affect the
-- current room or current items
prop_badCommandSameState :: CMQ.GameState -> Bool
prop_badCommandSameState state =
    CMQ.currRoom  nextState == CMQ.currRoom  state &&
    CMQ.currItems nextState == CMQ.currItems state
    where nextState = CMQ.badCommand state

-- Execute command accepts valid lowercase letters
prop_executeCommandLowercase :: Char -> CMQ.GameState -> Bool
prop_executeCommandLowercase char state
    | char `elem` "nsldih" = func state /= CMQ.badCommand state
    | otherwise            = True
    where func = CMQ.executeCommand [char]

-- Execute command accepts valid uppercase letters
prop_executeCommandUppercase :: Char -> CMQ.GameState -> Bool
prop_executeCommandUppercase char state
    | char `elem` "NSLDIH" = func state /= CMQ.badCommand state
    | otherwise            = True
    where func = CMQ.executeCommand [char]

return []

main :: IO Bool
main = $quickCheckAll
