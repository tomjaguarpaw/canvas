{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}

module Circle where

import qualified Data.Text.Lazy     as T
import qualified Control.Lens       as L
import           Doc                (Doc)
import qualified Doc                as D
import qualified Html               as H
import           Control.Monad      (guard)

data CircleEvent = MouseOver | MouseOut | MouseClick deriving Show
data CircleState = CircleState { _csHovered  :: Bool
                               , _csSelected :: Bool } deriving Show

data Circle = Circle { _cState :: CircleState } deriving Show
$(L.makeLenses ''Circle)
$(L.makeLenses ''CircleState)

parseCircleEvent :: T.Text -> Maybe CircleEvent
parseCircleEvent = \case "mouseover" -> Just MouseOver
                         "mouseout"  -> Just MouseOut
                         "click"     -> Just MouseClick
                         _           -> Nothing
                         -- The default case is actually "unexpected behaviour"
                         -- as we should never get an unknown event for any
                         -- given name

circleColor :: CircleState -> T.Text
circleColor c = case (L.view csHovered c, L.view csSelected c)
                of (True, True)   -> "#cc0000"
                   (True, False)  -> "#cccccc"
                   (False, True)  -> "#ff0000"
                   (False, False) -> "#ffffff"

circleMake :: Circle
circleMake = Circle (CircleState False False)

circleHandle :: CircleEvent -> Circle -> Circle
circleHandle MouseOver  = L.set  (cState.csHovered)  True
circleHandle MouseOut   = L.set  (cState.csHovered)  False
circleHandle MouseClick = L.over (cState.csSelected) not

guiCircle :: T.Text -> Circle -> H.GUICircle
guiCircle n c = H.GUICircle { H.gcName  = n
                            , H.gcColor = (circleColor . L.view cState) c }

-- TODO: duplication with button
circle :: Circle -> Doc [H.GUICircle] CircleEvent
circle c = D.Doc $ do
  n <- D.unique
  return ([guiCircle n c], parseMessage n)
  where parseMessage n message = case T.split (== ',') message
                                 of [theName, theEvent] -> do
                                      guard (theName == n)
                                      parseCircleEvent theEvent
                                    _ -> Nothing

circleC :: Circle -> Doc [H.GUICircle] (CircleEvent, Circle)
circleC = D.widgetHandler circleHandle circle

data Selected = Selected Circle deriving Show

data Unselected = Unselected Circle  deriving Show

selectedC :: Selected -> Doc [H.GUICircle] (CircleEvent, Selected)
selectedC = D.widgetHandler selectedHandle (circle . unselected)
  where unselected (Selected s) = s

unselectedC :: Unselected -> Doc [H.GUICircle] (CircleEvent, Unselected)
unselectedC = D.widgetHandler unselectedHandle (circle . ununselected)
  where ununselected (Unselected s) = s

unselectedHandle :: CircleEvent -> Unselected -> Unselected
unselectedHandle ev (Unselected c) = Unselected ((case ev of MouseClick -> id
                                                             _          -> circleHandle ev) c)

selectedHandle :: CircleEvent -> Selected -> Selected
selectedHandle ev (Selected c) = Selected ((case ev of MouseClick -> id
                                                       _          -> circleHandle ev) c)


selectedOfUnselected :: Unselected -> Selected
selectedOfUnselected (Unselected c) = Selected (L.set (cState.csSelected) True c)

unselectedOfSelected :: Selected -> Unselected
unselectedOfSelected (Selected c) = Unselected (L.set (cState.csSelected) False c)

selectedMake :: Selected
selectedMake = (Selected . L.set (cState.csSelected) True) circleMake

unselectedMake :: Unselected
unselectedMake = Unselected circleMake
