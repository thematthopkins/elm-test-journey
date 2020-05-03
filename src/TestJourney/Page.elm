module TestJourney.Page exposing
    ( Finder
    , FinderPart(..)
    , SelectorMultiple(..)
    , SelectorSingle(..)
    , multiple
    , multipleTestAttr
    , root
    , single
    , singleTestAttr
    )

import Html.Attributes as Attributes
import Test.Html.Selector as Selector


type SelectorMultiple
    = SelectorMultiple (List Selector.Selector)


type SelectorSingle
    = SelectorSingle (List Selector.Selector)


type FinderPart
    = FinderPartSingle SelectorSingle
    | FinderPartMultiple SelectorMultiple Int


type alias Finder =
    List FinderPart


root : Finder
root =
    []


single : Finder -> List Selector.Selector -> Finder
single parent selector =
    parent ++ [ FinderPartSingle (SelectorSingle selector) ]


multiple : Finder -> List Selector.Selector -> Int -> Finder
multiple parent selector index =
    parent ++ [ FinderPartMultiple (SelectorMultiple selector) index ]


singleTestAttr : Finder -> String -> Finder
singleTestAttr parent testAttr =
    single parent
        [ Selector.attribute (Attributes.attribute "data-test" testAttr)
        ]


multipleTestAttr : Finder -> String -> Int -> Finder
multipleTestAttr parent testAttr =
    multiple parent
        [ Selector.attribute (Attributes.attribute "data-test" testAttr)
        ]
