module TestJourney.Page exposing
    ( Finder
    , multiple
    , multipleRecord
    , multipleRecordTestAttr
    , multipleTestAttr
    , root
    , single
    , singleRecord
    , singleRecordTestAttr
    , singleTestAttr
    )

import Html.Attributes as Attributes
import Test.Html.Selector as Selector
import TestJourney.Internal as Internal exposing (FinderPart(..), FriendlyName)


type alias Finder =
    Internal.Finder


root : Finder
root =
    Internal.Finder []


single : Finder -> FriendlyName -> List Selector.Selector -> Finder
single (Internal.Finder parent) friendlyName selector =
    Internal.Finder (parent ++ [ FinderPartSingle friendlyName selector ])


singleRecord : Finder -> FriendlyName -> List Selector.Selector -> (Finder -> element) -> element
singleRecord parent friendlyName selector fn =
    single parent friendlyName selector
        |> fn


multiple : Finder -> FriendlyName -> List Selector.Selector -> Int -> Finder
multiple (Internal.Finder parent) friendlyName selector index =
    Internal.Finder (parent ++ [ FinderPartMultiple friendlyName selector index ])


multipleRecord : Finder -> FriendlyName -> List Selector.Selector -> (Finder -> element) -> Int -> element
multipleRecord parent friendlyName selector fn index =
    multiple parent friendlyName selector index
        |> fn


testAttrSelector : String -> List Selector.Selector
testAttrSelector testAttrVal =
    [ Selector.attribute (Attributes.attribute "data-test" testAttrVal)
    ]


singleTestAttr : Finder -> String -> Finder
singleTestAttr parent testAttr =
    single parent testAttr (testAttrSelector testAttr)


singleRecordTestAttr : Finder -> String -> (Finder -> element) -> element
singleRecordTestAttr parent testAttr fn =
    singleRecord parent testAttr (testAttrSelector testAttr) fn


multipleTestAttr : Finder -> String -> Int -> Finder
multipleTestAttr parent testAttr index =
    multiple parent testAttr (testAttrSelector testAttr) index


multipleRecordTestAttr : Finder -> String -> (Finder -> element) -> Int -> element
multipleRecordTestAttr parent testAttr fn index =
    multipleRecord parent testAttr (testAttrSelector testAttr) fn index
