module TodoExampleTest exposing (suite)

import Expect
import Html.Attributes exposing (..)
import Test exposing (..)
import TestJourney as J
import TodoExample
import TodoExamplePage exposing (page)


program : J.ProgramDefinition TodoExample.Model (TodoExample.Effect TodoExample.Msg) TodoExample.Msg
program =
    { view = TodoExample.view
    , update = TodoExample.update
    , initialModel = TodoExample.emptyModel
    , subscriptions = TodoExample.subscriptions
    , debugToString = Debug.toString
    }


suite : Test
suite =
    describe "TODO List"
        [ test "Add Item" <|
            \_ ->
                J.start program
                    |> J.input "myNewItem" page.addItemTextInput
                    |> J.dontSee page.addItemLoader
                    |> J.click page.addItemButton
                    |> J.see page.addItemLoader
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
                    |> J.dontSee page.addItemLoader
                    |> J.see page.addItemButton
                    |> J.see (page.items 0 |> .self)
                    |> J.seeText "myNewItem" (page.items 0 |> .label)
                    |> J.finish
        , test "Add Item 2" <|
            \_ ->
                J.start program
                    |> J.mapModel
                        (\m ->
                            m
                        )
                    |> J.input "myNewItem" page.addItemTextInput
                    |> J.dontSee page.addItemLoader
                    |> J.click page.addItemButton
                    |> J.see page.addItemLoader
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
                    |> J.dontSee page.addItemLoader
                    |> J.see page.addItemButton
                    |> J.finish
        ]
