module TestJourney.Internal exposing
    ( Finder(..)
    , FinderPart(..)
    , FriendlyName
    )

import Test.Html.Selector as Selector


type alias FriendlyName =
    String


type FinderPart
    = FinderPartSingle FriendlyName (List Selector.Selector)
    | FinderPartMultiple FriendlyName (List Selector.Selector) Int


type Finder
    = Finder (List FinderPart)

