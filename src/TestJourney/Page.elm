module TestJourney.Page exposing
    ( Finder
    , FinderPart(..)
    , SelectorMultiple(..)
    , SelectorSingle(..)
    , finderFriendlyName
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


type SelectorMultiple
    = SelectorMultiple (List Selector.Selector)


type SelectorSingle
    = SelectorSingle (List Selector.Selector)


type alias FriendlyName =
    String


type FinderPart
    = FinderPartSingle FriendlyName SelectorSingle
    | FinderPartMultiple FriendlyName SelectorMultiple Int


type alias Finder =
    List FinderPart


root : Finder
root =
    []


finderFriendlyName : Finder -> String
finderFriendlyName f =
    f
        |> List.map
            (\part ->
                case part of
                    FinderPartSingle name _ ->
                        name

                    FinderPartMultiple name _ _ ->
                        name
            )
        |> String.join "."


single : Finder -> FriendlyName -> List Selector.Selector -> Finder
single parent friendlyName selector =
    parent ++ [ FinderPartSingle friendlyName (SelectorSingle selector) ]


singleRecord : Finder -> FriendlyName -> List Selector.Selector -> (Finder -> element) -> element
singleRecord parent friendlyName selector fn =
    single parent friendlyName selector
        |> fn


multiple : Finder -> FriendlyName -> List Selector.Selector -> Int -> Finder
multiple parent friendlyName selector index =
    parent ++ [ FinderPartMultiple friendlyName (SelectorMultiple selector) index ]


multipleRecord : Finder -> FriendlyName -> List Selector.Selector -> (Finder -> element) -> Int -> element
multipleRecord parent friendlyName selector fn index =
    multiple parent (friendlyName ++ "[" ++ String.fromInt index ++ "]") selector index
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
