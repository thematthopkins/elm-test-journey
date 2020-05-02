module TodoExampleTest exposing (suite)

import Expect
import Html.Attributes exposing (..)
import Test exposing (..)
import Test.Html.Selector as Selector
import TestJourney as J
import TodoExample


program : J.ProgramDefinition TodoExample.Model (TodoExample.Effect TodoExample.Msg) TodoExample.Msg
program =
    { view = TodoExample.view
    , update = TodoExample.update
    , subscriptions = TodoExample.subscriptions
    , debugToString = Debug.toString
    }


suite : Test
suite =
    describe "TODO List"
        [ test "Add Item" <|
            \_ ->
                J.start program TodoExample.emptyModel
                    |> J.input "myNewItem" (J.findSingle [ Selector.attribute (attribute "data-test" "add-item-text") ])
                    |> J.dontSee (J.findSingle [ Selector.attribute (attribute "data-test" "add-item-loader") ])
                    |> J.click (J.findSingle [ Selector.attribute (attribute "data-test" "add-item-button") ])
                    |> J.see (J.findSingle [ Selector.attribute (attribute "data-test" "add-item-loader") ])
                    |> J.handleEffect
                        (\effect ->
                            case effect of
                                TodoExample.EffectAddItem msg input ->
                                    Just
                                        ( msg (Ok (TodoExample.TodoItemID 55))
                                        , Expect.equal input "myNewItem"
                                        )

                                _ ->
                                    Nothing
                        )
                    |> J.dontSee (J.findSingle [ Selector.attribute (attribute "data-test" "add-item-loader") ])
                    |> J.see (J.findSingle [ Selector.attribute (attribute "data-test" "add-item-button") ])
                    |> J.finish
        ]
