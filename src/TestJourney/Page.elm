module TestJourney.Page exposing
    ( root, multiple, multipleRecord, single, singleRecord
    , Element, Finder
    , multipleTestAttr, multipleRecordTestAttr, singleTestAttr, singleRecordTestAttr
    )

{-| Create Page Objects for `elm-test-journey`.


## Building the Page Model

@docs root, multiple, multipleRecord, single, singleRecord


## Types

@docs Element, Finder


## Helpers

In practice, a convention like "all elements are identified by an `"data-test"="myTestIdentifier"`" makes page objects
more resiliant, and makes it clear when a node in the application under test has test dependencies. These helpers make
creating Page Objects targeting that convention simpler. If your project has it's own conventions, create your own set of
helpers.

@docs multipleTestAttr, multipleRecordTestAttr, singleTestAttr, singleRecordTestAttr

-}

import Html.Attributes as Attributes
import Test.Html.Selector as Selector
import TestJourney.Internal as Internal exposing (FinderPart(..), FriendlyName)


{-| An element represents a series of selectors and functions that result in finding a single specific node in your application under test. When defining a record element, via [`singleRecord`](#singleRecord), or [`multipleRecord`](#multipleRecord), ensure that you always have a field named `self`. `self` allows elements with and without other fields defined to be treated uniformly.
-}
type alias Element children =
    { children | self : Finder }


{-|

    Used by [`TestJourney` ](../../TestJourney.elm) to help locate [`Elements`](#Element) defined in your page.

-}
type alias Finder =
    Internal.Finder


{-| Sets up the context for the rest of the page object. An empty selector will match the root of the view (usually the body tag).

    page =
        root []
            (\rootElement ->
                { self = rootElement
                , myChildElement = singleTestAttr rootElement "myChildElement"
                }
            )

-}
root : List Selector.Selector -> (Finder -> Element children) -> Element children
root selector fn =
    fn (Internal.Finder [ FinderPartSingle "" selector ])


{-| Creates an ['Element'](#Element) referencing a single node.

    page =
        root
            []
            (\rootElement ->
                { self = rootElement
                , myChildElement = single rootElement "myChildElement" [ Selector.attribute (Attributes.attribute "data-test" "myChildElement") ]
                }
            )

    element =
        page.myChildElement

-}
single : Finder -> FriendlyName -> List Selector.Selector -> Element {}
single (Internal.Finder parent) friendlyName selector =
    { self = Internal.Finder (parent ++ [ FinderPartSingle friendlyName selector ]) }


{-| Creates an ['Element'](#Element) referencing a single node, w/ custom children.

    page =
        root []
            (\rootElement ->
                { self = rootElement
                , myElementWithChildren =
                    singleRecord rootElement
                        "myChildElement"
                        [ Selector.attribute (Attributes.attribute "data-test" "myChildElement") ]
                        (\myElementWithChildren ->
                            { self = myElementWithChildren
                            , childOfChild = singleTestAttr myElementWithChildren "childOfChild"
                            }
                        )
                }
            )

    element =
        page.myElementWithChildren.childOfChild

-}
singleRecord : Finder -> FriendlyName -> List Selector.Selector -> (Finder -> Element children) -> Element children
singleRecord (Internal.Finder parent) friendlyName selector fn =
    Internal.Finder (parent ++ [ FinderPartSingle friendlyName selector ])
        |> fn


{-| Represents a list of nodes.

    page =
        root []
            (\rootElement ->
                { self = rootElement
                , myListItem =
                    multiple rootElement
                        "myListItem"
                        [ Selector.attribute (Attributes.attribute "data-test" "myListItem")
                        ]
                }
            )

    element =
        page.myListItem 1

-}
multiple : Finder -> FriendlyName -> List Selector.Selector -> Int -> Element {}
multiple (Internal.Finder parent) friendlyName selector index =
    { self = Internal.Finder (parent ++ [ FinderPartMultiple friendlyName selector index ]) }


{-| Represents a list of nodes w/ custom children.

    page =
        root []
            (\rootElement ->
                { self = rootElement
                , myListItem =
                    multipleRecord rootElement
                        "myListItem"
                        [ Selector.attribute
                            (Attributes.attribute "data-test" "myListItem")
                        ]
                        (\myListItem ->
                            { self = myListItem
                            , label = singleTestAttr myListItem "itemLabel"
                            }
                        )
                }
            )

    element =
        page.myListItem 0 |> .label

-}
multipleRecord :
    Finder
    -> FriendlyName
    -> List Selector.Selector
    -> (Finder -> Element children)
    -> Int
    -> Element children
multipleRecord (Internal.Finder parent) friendlyName selector fn index =
    Internal.Finder (parent ++ [ FinderPartMultiple friendlyName selector index ])
        |> fn


testAttrSelector : String -> List Selector.Selector
testAttrSelector testAttrVal =
    [ Selector.attribute (Attributes.attribute "data-test" testAttrVal)
    ]


{-| Creates an ['Element'](#Element) referencing a single node, following the "data-test" attribute convention.

    page =
        root
            []
            (\rootElement ->
                { self = rootElement
                , myChildElement = singleTestAttr rootElement "myChildElement"
                }
            )

    element =
        page.myChildElement

-}
singleTestAttr : Finder -> String -> Element {}
singleTestAttr parent testAttr =
    single parent testAttr (testAttrSelector testAttr)


{-| Creates an ['Element'](#Element) referencing a single node, w/ custom children, following the "data-test" attribute convention.

    page =
        root []
            (\rootElement ->
                { self = rootElement
                , myElementWithChildren =
                    singleRecord rootElement
                        "myChildElement"
                        (\myElementWithChildren ->
                            { self = myElementWithChildren
                            , childOfChild = singleTestAttr myElementWithChildren "childOfChild"
                            }
                        )
                }
            )

    element =
        page.myElementWithChildren.childOfChild

-}
singleRecordTestAttr : Finder -> String -> (Finder -> Element children) -> Element children
singleRecordTestAttr parent testAttr fn =
    singleRecord parent testAttr (testAttrSelector testAttr) fn


{-| Represents a list of nodes, following the "data-test" attribute convention.

    page =
        root []
            (\rootElement ->
                { self = rootElement
                , myListItem =
                    multipleTestAttr rootElement "myListItem"
                }
            )

    element =
        page.myListItem 1

-}
multipleTestAttr : Finder -> String -> Int -> Element {}
multipleTestAttr parent testAttr index =
    multiple parent testAttr (testAttrSelector testAttr) index


{-| Represents a list of nodes w/ custom children, following the "data-test" attribute convention.

    page =
        root []
            (\rootElement ->
                { self = rootElement
                , myListItem =
                    multipleRecord rootElement
                        "myListItem"
                        (\myListItem ->
                            { self = myListItem
                            , label = singleTestAttr myListItem "itemLabel"
                            }
                        )
                }
            )

    element =
        page.myListItem 0 |> .label

-}
multipleRecordTestAttr : Finder -> String -> (Finder -> Element children) -> Int -> Element children
multipleRecordTestAttr parent testAttr fn index =
    multipleRecord parent testAttr (testAttrSelector testAttr) fn index
