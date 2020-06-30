module TodoExample exposing
    ( Effect(..)
    , Model
    , Msg(..)
    , TodoItemID(..)
    , TodoItemStatus(..)
    , emptyModel
    , main
    , subscriptions
    , update
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Process
import Task
import Time


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = cmdUpdate
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type Msg
    = AddItem String
    | NewItemLabelUpdated String
    | AddItemComplete String (Result Http.Error TodoItemID)
    | TodoItemMsg TodoItemID TodoItemMsg


type TodoItemMsg
    = Remove
    | RemoveComplete (Result Http.Error ())
    | UpdateStatus TodoItemStatus
    | UpdateStatusComplete { priorStatus : TodoItemStatus, result : Result Http.Error () }


type Effect msg
    = EffectAddItem (Result Http.Error TodoItemID -> msg) String
    | EffectRemoveItem (Result Http.Error () -> msg) TodoItemID
    | EffectUpdateItemStatus (Result Http.Error () -> msg) TodoItemID TodoItemStatus


mapEffect : (a -> msg) -> Effect a -> Effect msg
mapEffect fn effect =
    case effect of
        EffectAddItem m a ->
            EffectAddItem (m >> fn) a

        EffectRemoveItem m a ->
            EffectRemoveItem (m >> fn) a

        EffectUpdateItemStatus m a b ->
            EffectUpdateItemStatus (m >> fn) a b


mapEffects : (a -> msg) -> Effects a -> Effects msg
mapEffects fn =
    List.map (mapEffect fn)


type alias Effects msg =
    List (Effect msg)


type TodoItemID
    = TodoItemID Int


type TodoItemStatus
    = TodoItemComplete
    | TodoItemIncomplete


type alias TodoItem =
    { id : TodoItemID
    , label : String
    , status : TodoItemStatus
    , isRemoving : Bool
    }


emptyModel : Model
emptyModel =
    { items = []
    , isAdding = False
    , newItemLabel = ""
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( emptyModel
    , Cmd.none
    )


type alias Model =
    { items : List TodoItem
    , isAdding : Bool
    , newItemLabel : String
    }


cmdUpdate : Msg -> Model -> ( Model, Cmd Msg )
cmdUpdate msg model =
    let
        ( newModel, effects ) =
            update msg model
    in
    ( newModel
    , effects
        |> List.map toCmd
        |> Cmd.batch
    )


updateTodoItem : TodoItemMsg -> TodoItem -> ( Maybe TodoItem, Effects TodoItemMsg )
updateTodoItem msg model =
    case msg of
        Remove ->
            ( Just { model | isRemoving = True }, [ EffectRemoveItem RemoveComplete model.id ] )

        RemoveComplete result ->
            case result of
                Err _ ->
                    ( Just model, [] )

                Ok _ ->
                    ( Nothing
                    , []
                    )

        UpdateStatus status ->
            ( Just { model | status = status }
            , [ EffectUpdateItemStatus (\result -> UpdateStatusComplete { priorStatus = model.status, result = result }) model.id status ]
            )

        UpdateStatusComplete { priorStatus, result } ->
            case result of
                Err _ ->
                    ( Just { model | status = priorStatus }, [] )

                Ok _ ->
                    ( Just model, [] )


update : Msg -> Model -> ( Model, Effects Msg )
update msg model =
    case msg of
        AddItem label ->
            ( { model | isAdding = True }
            , [ EffectAddItem (AddItemComplete label) label ]
            )

        NewItemLabelUpdated label ->
            ( { model | newItemLabel = label }
            , []
            )

        AddItemComplete label result ->
            case result of
                Ok newItemID ->
                    ( { model
                        | isAdding = False
                        , newItemLabel = ""
                        , items =
                            model.items
                                ++ [ { id = newItemID
                                     , label = label
                                     , status = TodoItemIncomplete
                                     , isRemoving = False
                                     }
                                   ]
                      }
                    , []
                    )

                Err _ ->
                    ( { model | isAdding = False }
                    , []
                    )

        TodoItemMsg id itemMsg ->
            let
                ( newItems, effects ) =
                    model.items
                        |> List.foldr
                            (\item ( itemsAccum, effectsAccum ) ->
                                if item.id == id then
                                    let
                                        ( newItem, effect ) =
                                            updateTodoItem itemMsg item

                                        newItemsAccum =
                                            case newItem of
                                                Just i ->
                                                    i :: itemsAccum

                                                Nothing ->
                                                    itemsAccum
                                    in
                                    ( newItemsAccum, effect ++ effectsAccum )

                                else
                                    ( item :: itemsAccum, effectsAccum )
                            )
                            ( [], [] )
            in
            ( { model | items = newItems }
            , effects
                |> mapEffects (TodoItemMsg id)
            )


viewItem : TodoItem -> Html TodoItemMsg
viewItem item =
    div [ attribute "data-test" "todo-item" ]
        [ input
            [ type_ "checkbox"
            , checked (item.status == TodoItemComplete)
            , onCheck
                (\checked ->
                    UpdateStatus
                        (if checked then
                            TodoItemComplete

                         else
                            TodoItemIncomplete
                        )
                )
            ]
            []
        , span [ attribute "data-test" "item-label" ]
            [ text item.label
            ]
        , if item.isRemoving then
            span [ attribute "data-test" "item-remove-processing" ]
                [ text "removing..."
                ]

          else
            button [ attribute "data-test" "item-remove-button", onClick Remove ]
                [ text "Remove"
                ]
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Todo List"
    , body =
        [ div [ style "text-align" "center" ]
            [ h1 []
                [ text "Todo List"
                ]
            , div []
                (model.items
                    |> List.map
                        (\item ->
                            item
                                |> viewItem
                                |> Html.map (TodoItemMsg item.id)
                        )
                )
            , div []
                [ input [ type_ "text",
                    onInput NewItemLabelUpdated,
                    value model.newItemLabel,
                    attribute "data-test" "add-item-text-input",
                    classList [("is-processing-addition", model.isAdding)]
                ] []
                , if model.isAdding then
                    div [ attribute "data-test" "add-item-loader" ] [ text "loading..." ]

                  else
                    button [ onClick (AddItem model.newItemLabel), attribute "data-test" "add-item-button" ]
                        [ text "Add Item"
                        ]
                ]
            ]
        ]
    }


toCmd : Effect msg -> Cmd msg
toCmd e =
    case e of
        EffectAddItem msg _ ->
            Process.sleep 2000.0
                |> Task.andThen (\_ -> Time.now)
                |> Task.perform
                    (\time ->
                        let
                            newID =
                                TodoItemID (Time.posixToMillis time)
                        in
                        msg (Ok newID)
                    )

        EffectRemoveItem msg _ ->
            Process.sleep 2.0
                |> Task.perform (\_ -> msg (Ok ()))

        EffectUpdateItemStatus msg _ _ ->
            Process.sleep 2.0
                |> Task.perform (\_ -> msg (Ok ()))
