module TodoExamplePage exposing (Page, page)

import Html.Attributes exposing (..)
import TestJourney.Page as P


type alias Page =
    P.Element
        { addItemButton : P.Element {}
        , addItemLoader : P.Element {}
        , addItemTextInput : P.Element {}
        , items :
            Int
            ->
                P.Element
                    { complete : P.Element {}
                    , label : P.Element {}
                    , removeButton : P.Element {}
                    , removeProcessing : P.Element {}
                    }
        }


page : Page
page =
    P.root []
        (\root ->
            { self = root
            , addItemLoader = P.singleTestAttr root "add-item-loader"
            , addItemButton = P.singleTestAttr root "add-item-button"
            , addItemTextInput = P.singleTestAttr root "add-item-text-input"
            , items =
                P.multipleRecordTestAttr root
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
        )
