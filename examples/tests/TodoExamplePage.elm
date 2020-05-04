module TodoExamplePage exposing (page)

import Html.Attributes exposing (..)
import TestJourney.Page as P


page =
    { addItemLoader = P.singleTestAttr P.root "add-item-loader"
    , addItemButton = P.singleTestAttr P.root "add-item-button"
    , addItemTextInput = P.singleTestAttr P.root "add-item-text-input"
    , items =
        P.multipleRecordTestAttr P.root
            "todo-item"
            (\item ->
                { self = item
                , label = P.singleTestAttr item "item-label"
                , removeButton = P.singleTestAttr item "item-remove-button"
                , removeProcessing = P.singleTestAttr item "item-remove-processing"
                , complete = P.singleTestAttr item "item-complete"
                }
            )
    }
