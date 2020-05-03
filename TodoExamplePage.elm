module TodoExamplePage exposing (page)

import Html.Attributes exposing (..)
import Test.Html.Selector as Selector
import TestJourney exposing (multiple, root, single)


page =
    let
        r =
            root
    in
    { addItemLoader =
        root
            :: [ single [ Selector.attribute (attribute "data-test" "add-item-loader") ]
               ]
    , addItem =
        root
            :: [ single [ Selector.attribute (attribute "data-test" "add-item-button") ]
               ]
    , addItemTextInput =
        root
            :: [ single [ Selector.attribute (attribute "data-test" "add-item-text-input") ]
               ]
    }
