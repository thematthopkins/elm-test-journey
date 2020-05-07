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
                                    J.EffectProcessed
                                        (Expect.equal
                                            input
                                            "myNewItem"
                                        )
                                        (msg
                                            (Ok (TodoExample.TodoItemID 55))
                                        )

                                _ ->
                                    J.EffectUnexpected
                        )
                    |> J.dontSee page.addItemLoader
                    |> J.see page.addItemButton
                    |> J.seeCount 1 page.items
                    |> J.see (page.items 0)
                    |> J.seeText "myNewItem" (page.items 0 |> .label)
                    |> J.finish
        , test "Remove Item" <|
            \_ ->
                J.start program
                    |> J.mapModel
                        (\m ->
                            { m
                                | items =
                                    [ { id = TodoExample.TodoItemID 22
                                      , label = "My Item To Remain"
                                      , status = TodoExample.TodoItemIncomplete
                                      , isRemoving = False
                                      }
                                    , { id = TodoExample.TodoItemID 23
                                      , label = "My Item To Remove"
                                      , status = TodoExample.TodoItemIncomplete
                                      , isRemoving = False
                                      }
                                    ]
                            }
                        )
                    |> J.seeCount 2 page.items
                    |> J.seeText "My Item To Remove" (page.items 1 |> .label)
                    |> J.dontSee (page.items 1 |> .removeProcessing)
                    |> J.click (page.items 1 |> .removeButton)
                    |> J.see (page.items 1 |> .removeProcessing)
                    |> J.handleEffect
                        (\effect ->
                            case effect of
                                TodoExample.EffectRemoveItem msg id ->
                                    J.EffectProcessed
                                        (Expect.equal
                                            id
                                            (TodoExample.TodoItemID 23)
                                        )
                                        (msg
                                            (Ok ())
                                        )

                                _ ->
                                    J.EffectUnexpected
                        )
                    |> J.seeCount 1 page.items
                    |> J.seeText "My Item To Remain" (page.items 0 |> .label)
                    |> J.finish
        ]
