module TodoExamplePage exposing (page)

import Html.Attributes exposing (..)
import TestJourney.Page exposing (multipleTestAttr, root, singleTestAttr)


page =
    let
        r =
            root
    in
    { addItemLoader = singleTestAttr r "add-item-loader"
    , addItemButton = singleTestAttr r "add-item-button"
    , addItemTextInput = singleTestAttr r "add-item-text-input"
    }
