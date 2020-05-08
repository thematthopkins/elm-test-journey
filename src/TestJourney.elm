module TestJourney exposing
    ( start, ProgramDefinition, finish
    , mapModel, expectModel
    , handleEffect, EffectHandlerResult(..)
    , injectMsg
    , blur, check, click, custom, doubleClick, focus, input, mouseDown, mouseEnter, mouseLeave, mouseOut, mouseOver, mouseUp, submit, uncheck
    , see, dontSee, seeCount, seeText
    )

{-| Write easy-to-maintain acceptance-like tests.


## Setup

@docs start, ProgramDefinition, finish


## Direct model access

@docs mapModel, expectModel


## Effects

@docs handleEffect, EffectHandlerResult


## Messages

@docs injectMsg


## Html Events

@docs blur, check, click, custom, doubleClick, focus, input, mouseDown, mouseEnter, mouseLeave, mouseOut, mouseOver, mouseUp, submit, uncheck


## Expectations

To be used with your [`page`](../Page#page)

@docs see, dontSee, seeCount, seeText

-}

import Browser
import Expect
import Html
import Json.Encode
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner
import Test.Runner.Failure
import TestJourney.Internal exposing (Finder(..), FinderPart(..))
import TestJourney.Page as Page


{-| Defines the application under test. This mirrors your elm `Program`, but without subscriptions or init, and `List effect` instead of `Cmd`.

`debugToString` should always be `Debug.toString`. Having the caller supply this allows `elm-test-program` to avoid it's use, and be published as a package.

-}
type alias ProgramDefinition model effect msg =
    { view : model -> Browser.Document msg
    , update : msg -> model -> ( model, List effect )
    , initialModel : model
    , debugToString : effect -> String
    }


{-| Call at the start of your test pipeline to kick things off.
-}
start : ProgramDefinition model effect msg -> ProgramState model effect msg
start programDefinition =
    { model = programDefinition.initialModel
    , definition = programDefinition
    , pendingEffects = []
    , result = Ok []
    }


{-| Call at the end of your test. Automatically add an expectations that there are no
pending effects left to process, since that is usually inadvertant in practice.
-}
finish : ProgramState model effect msg -> Expect.Expectation
finish program =
    case program.result of
        Err e ->
            failureToExpectation e

        Ok stepsProcessed ->
            if stepsProcessed == [] then
                Expect.fail "Empty test failed - no steps or expectations provided"

            else if program.pendingEffects /= [] then
                Expect.fail
                    ("Test finished with "
                        ++ String.fromInt (List.length program.pendingEffects)
                        ++ " pending expectations that were left unhandled:\n"
                        ++ (program.pendingEffects
                                |> List.map program.definition.debugToString
                                |> List.map (\s -> "    " ++ s)
                                |> String.join "\n"
                           )
                        ++ "\nCompleted steps:\n"
                        ++ (stepsProcessed
                                |> List.map
                                    (\s ->
                                        "    [Passed] " ++ s
                                    )
                                |> String.join "\n"
                           )
                    )

            else
                Expect.pass


{-| Directly transform the underlying model. This can be useful for altering your [`initialModel`](#ProgramDefinition) supplied in [`ProgramDefinition`](#ProgramDefinition). This should generally be avoided, in the body of your tests.
-}
mapModel : (model -> model) -> ProgramState model effect msg -> ProgramState model effect msg
mapModel fn =
    step "mapModel" (\p -> Ok ( fn p.model, [] ))


{-| Directly run an expectation on the current model of the application under tests. Useful when there are changes that aren't represented in the UI (e.g. I want to make sure I get a new session id after logging in).
-}
expectModel : (model -> Expect.Expectation) -> ProgramState model effect msg -> ProgramState model effect msg
expectModel fn =
    staticStep "expectModel" (\p -> fn p.model)


{-| Processes the next effect waiting to be processed.

This is usually simulating the other end of your effect. In the case of an effect representing an HTTP Cmd, this takes place of the server. For an out port, this plays the role simulating the javascript code.

        -- Application Under Test
        type alias ItemID = Int

        type Msg =
            ItemAdded ItemID

        type MyEffect =
            EffectAddItem (Result Http.Error ItemID) String
            EffectRemoveItem ItemID


        -- Test
        handleAddItem effect =
            case effect of
                EffectAddItem msg label ->
                    EffectProcessed
                        (Expect.equal
                            input
                            "myLabel"
                        )
                        (msg
                            (Ok ItemID 55))
                        )

                _ ->
                    EffectUnexpected

-}
handleEffect : (effect -> EffectHandlerResult msg) -> ProgramState model effect msg -> ProgramState model effect msg
handleEffect fn programState =
    let
        nextEffect =
            programState.pendingEffects
                |> List.head
                |> Maybe.map (\e -> "(" ++ programState.definition.debugToString e ++ ")")
                |> Maybe.withDefault ""
    in
    step ("handleEffect " ++ nextEffect) (handleEffectStep fn) programState


{-| The results returned by [`handleEffect`](#handleEffect). Returns either an `EffectProcessed` w/ an `Expectation` and the msg to be sent to the `update` function of the application under test.
-}
type EffectHandlerResult msg
    = EffectProcessed Expect.Expectation msg
    | EffectUnexpected


handleEffectStep : (effect -> EffectHandlerResult msg) -> Step model effect msg
handleEffectStep fn program =
    case program.pendingEffects of
        effect :: _ ->
            let
                result =
                    fn effect
            in
            case result of
                EffectProcessed expect msg ->
                    let
                        failure =
                            expectationToFailure expect
                    in
                    case failure of
                        Just f ->
                            Err f

                        Nothing ->
                            Ok (program.definition.update msg program.model)

                EffectUnexpected ->
                    Err (failureFromDescription ("Unhandled effect: " ++ program.definition.debugToString effect))

        [] ->
            Err (failureFromDescription "attempted to handle effect when no effects were generated")


type alias ProgramState model effect msg =
    { model : model
    , definition : ProgramDefinition model effect msg
    , pendingEffects : List effect
    , result : Result Failure (List StepDescription)
    }


{-| Sends the supplied `msg` into appliication under test. This is how `ports` and `Subscriptions` should be simulated.
-}
injectMsg : msg -> ProgramState model effect msg -> ProgramState model effect msg
injectMsg msg =
    step "injectMsg" (\p -> Ok (p.definition.update msg p.model))



-- Html Events


{-| Ensures the target element exists exactly once, simulates a click event, and processes the generated message.
-}
click : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
click finder =
    step ("click " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.click finder.self)


{-| Ensures the target element exists exactly once, simulates a input event w/ the given text, and processes the generated message.
-}
input : String -> Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
input text finder =
    step ("input \"" ++ text ++ "\" on " ++ finderFriendlyName finder.self)
        (simulateEventStep (Event.input text) finder.self)


{-| Ensures the target element exists exactly once, simulates a custom, and processes the generated message.

                        See (see [Test.Html.Event.custom](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test-Html-Event#custom))

-}
custom : String -> Json.Encode.Value -> Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
custom eventName value finder =
    step ("custom \"" ++ eventName ++ "\" sent \"" ++ Json.Encode.encode 4 value ++ "\" on " ++ finderFriendlyName finder.self)
        (simulateEventStep (Event.custom eventName value) finder.self)


{-| Ensures the target element exists exactly once, simulates a double-click event, and processes the generated message.
-}
doubleClick : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
doubleClick finder =
    step ("doubleClick " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.doubleClick finder.self)


{-| Ensures the target element exists exactly once, simulates a mouseDown event, and processes the generated message.
-}
mouseDown : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseDown finder =
    step ("mouseDown " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseDown finder.self)


{-| Ensures the target element exists exactly once, simulates a mouseUp event, and processes the generated message.
-}
mouseUp : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseUp finder =
    step ("mouseUp " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseUp finder.self)


{-| Ensures the target element exists exactly once, simulates a mouseEnter event, and processes the generated message.
-}
mouseEnter : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseEnter finder =
    step ("mouseEnter " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseEnter finder.self)


{-| Ensures the target element exists exactly once, simulates a mouseLeave event, and processes the generated message.
-}
mouseLeave : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseLeave finder =
    step ("mouseLeave " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseLeave finder.self)


{-| Ensures the target element exists exactly once, simulates a mouseOver event, and processes the generated message.
-}
mouseOver : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseOver finder =
    step ("mouseOver " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseOver finder.self)


{-| Ensures the target element exists exactly once, simulates a mouseOut event, and processes the generated message.
-}
mouseOut : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseOut finder =
    step ("mouseOut " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseOut finder.self)


{-| Ensures the target element exists exactly once, simulates a check true event, and processes the generated message.
-}
check : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
check finder =
    step ("check " ++ finderFriendlyName finder.self)
        (simulateEventStep (Event.check True) finder.self)


{-| Ensures the target element exists exactly once, simulates an check false event, and processes the generated message.
-}
uncheck : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
uncheck finder =
    step ("uncheck " ++ finderFriendlyName finder.self)
        (simulateEventStep (Event.check False) finder.self)


{-| Ensures the target element exists exactly once, simulates a submit event, and processes the generated message.
-}
submit : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
submit finder =
    step ("submit " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.submit finder.self)


{-| Ensures the target element exists exactly once, simulates a blur event, and processes the generated message.
-}
blur : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
blur finder =
    step ("blur " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.blur finder.self)


{-| Ensures the target element exists exactly once, simulates a focus event, and processes the generated message.
-}
focus : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
focus finder =
    step ("focus " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.focus finder.self)



-- Expectations


{-| Expect exactly a given number of matching elements to exist.
-}
seeCount : Int -> (Int -> Page.Element children) -> ProgramState model effect msg -> ProgramState model effect msg
seeCount expectedCount finderFn model =
    let
        (Finder finder) =
            finderFn 0
                |> .self
    in
    case List.reverse finder of
        [] ->
            { model | result = Err (failureFromDescription "Invalid finder for seeCount") }

        lastPart :: rest ->
            let
                parentFinder =
                    List.reverse rest
            in
            case lastPart of
                FinderPartMultiple name selector _ ->
                    staticStep
                        ("seeCount "
                            ++ String.fromInt expectedCount
                            ++ " of "
                            ++ finderFriendlyName
                                (Finder
                                    (parentFinder
                                        ++ [ FinderPartSingle name selector ]
                                    )
                                )
                        )
                        (seeCountStep expectedCount (Finder parentFinder) selector)
                        model

                _ ->
                    { model | result = Err (failureFromDescription "Invalid single finder for seeCount") }


seeCountStep : Int -> Page.Finder -> List Selector.Selector -> ProgramState model effect msg -> Expect.Expectation
seeCountStep expectedCount parentFinder lastFinderPart program =
    program.model
        |> program.definition.view
        |> .body
        |> Html.node "body" []
        |> Query.fromHtml
        |> resolveFinder parentFinder
        |> Result.andThen
            (\parent ->
                parent
                    |> Query.findAll lastFinderPart
                    |> Query.count
                        (\c ->
                            if c == expectedCount then
                                Expect.pass

                            else
                                Expect.fail
                                    ("Expected "
                                        ++ String.fromInt expectedCount
                                        ++ " matches found "
                                        ++ String.fromInt c
                                    )
                        )
                    |> expectationToResult ()
            )
        |> resultToExpectation


{-| Expect a given element to exist exactly once.
-}
see : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
see finder =
    staticStep ("see " ++ finderFriendlyName finder.self) (seeStep finder.self)


{-| Expect a given element to have a descendant w/ the given text.
-}
seeText : String -> Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
seeText expectText element =
    let
        (Finder finder) =
            element.self
    in
    staticStep ("seeText \"" ++ expectText ++ "\" at " ++ finderFriendlyName (Finder finder))
        (seeStep
            (Finder
                (finder ++ [ FinderPartSingle "" [ Selector.text expectText ] ])
            )
        )


{-| Expect a given element to not exist. If the target is a sequence of page finders (e.g. `parent.child.grandchild`), `dontSee` only passes if
`parent`, and `child` exist, but `grandchild` does not. Otherwise, it's too easy for `dontSee` to inadvertantly pass.
-}
dontSee : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
dontSee finder =
    staticStep ("dontSee " ++ finderFriendlyName finder.self) (dontSeeStep finder.self)


resolveFinder : Page.Finder -> Query.Single msg -> Result Failure (Query.Single msg)
resolveFinder (Finder finder) query =
    List.foldl
        (\finderPart parentResult ->
            parentResult
                |> Result.andThen
                    (\parent ->
                        case finderPart of
                            FinderPartSingle friendlyName selector ->
                                let
                                    failure =
                                        parent
                                            |> Expect.all
                                                [ Query.has selector
                                                , \p ->
                                                    p
                                                        |> Query.findAll selector
                                                        |> Query.count
                                                            (\count ->
                                                                if count == 1 then
                                                                    Expect.pass

                                                                else
                                                                    Expect.fail
                                                                        ("Expected to find just 1 "
                                                                            ++ friendlyName
                                                                            ++ " but found "
                                                                            ++ String.fromInt count
                                                                        )
                                                            )
                                                ]
                                            |> expectationToFailure
                                in
                                case failure of
                                    Nothing ->
                                        Ok (Query.find selector parent)

                                    Just f ->
                                        Err f

                            FinderPartMultiple _ selector index ->
                                let
                                    failure =
                                        parent
                                            |> Query.has selector
                                            |> expectationToFailure
                                in
                                case failure of
                                    Nothing ->
                                        Ok
                                            (parent
                                                |> Query.findAll selector
                                                |> Query.index index
                                            )

                                    Just f ->
                                        Err f
                    )
        )
        (Ok query)
        finder


seeStep : Page.Finder -> ProgramState model effect msg -> Expect.Expectation
seeStep finder program =
    program.model
        |> program.definition.view
        |> .body
        |> Html.node "body" []
        |> Query.fromHtml
        |> resolveFinder finder
        |> resultToExpectation


dontSeeStep : Page.Finder -> ProgramState model effect msg -> Expect.Expectation
dontSeeStep (Finder finder) program =
    finder
        |> List.reverse
        |> (\reversed ->
                case reversed of
                    [] ->
                        Expect.fail "No selector provided"

                    lastFinder :: rest ->
                        let
                            parentFinder =
                                List.reverse rest
                        in
                        dontSeeChildStep (Finder parentFinder) lastFinder program
           )


dontSeeChildStep : Page.Finder -> FinderPart -> ProgramState model effect msg -> Expect.Expectation
dontSeeChildStep parentFinder childFinder program =
    let
        parentResult =
            program.model
                |> program.definition.view
                |> .body
                |> Html.node "body" []
                |> Query.fromHtml
                |> resolveFinder parentFinder
    in
    case parentResult of
        Ok parent ->
            case childFinder of
                FinderPartSingle _ selector ->
                    parent
                        |> Query.hasNot selector

                FinderPartMultiple _ selector index ->
                    parent
                        |> Query.findAll selector
                        |> Query.index index
                        |> Query.has []
                        |> expectationToFailure
                        |> (\failureToFind ->
                                case failureToFind of
                                    Just _ ->
                                        Expect.pass

                                    Nothing ->
                                        parent
                                            |> Query.hasNot selector
                                            |> prependExpectationDescription
                                                ("found at least "
                                                    ++ String.fromInt (index + 1)
                                                    ++ "matching selector\n"
                                                )
                           )

        Err failure ->
            failureToExpectation failure


finderFriendlyName : Finder -> String
finderFriendlyName (Finder f) =
    f
        |> List.map
            (\part ->
                case part of
                    FinderPartSingle name _ ->
                        name

                    FinderPartMultiple name _ index ->
                        name ++ "[" ++ String.fromInt index ++ "]"
            )
        |> String.join "."


type alias Failure =
    { given : Maybe String
    , description : String
    , reason : Test.Runner.Failure.Reason
    }


failureFromDescription : String -> Failure
failureFromDescription description =
    { given = Nothing, description = description, reason = Test.Runner.Failure.Custom }


expectationToResult : a -> Expect.Expectation -> Result Failure a
expectationToResult onPass expectation =
    case expectationToFailure expectation of
        Nothing ->
            Ok onPass

        Just f ->
            Err f


resultToExpectation : Result Failure a -> Expect.Expectation
resultToExpectation result =
    case result of
        Ok _ ->
            Expect.pass

        Err failure ->
            failureToExpectation failure


prependExpectationDescription : String -> Expect.Expectation -> Expect.Expectation
prependExpectationDescription description expectation =
    case expectationToFailure expectation of
        Nothing ->
            Expect.pass

        Just f ->
            failureToExpectation { f | description = description ++ f.description }


expectationToFailure : Expect.Expectation -> Maybe Failure
expectationToFailure =
    Test.Runner.getFailureReason


failureToExpectation : Failure -> Expect.Expectation
failureToExpectation f =
    Expect.fail (Test.Runner.Failure.format f.description f.reason)


type alias StepDescription =
    String


type alias Step model effect msg =
    ProgramState model effect msg -> Result Failure ( model, List effect )


type alias StaticStep model effect msg =
    ProgramState model effect msg -> Expect.Expectation


simulateEventStep : ( String, Json.Encode.Value ) -> Finder -> Step model effect msg
simulateEventStep event finder program =
    program.model
        |> program.definition.view
        |> .body
        |> Html.node "body" []
        |> Query.fromHtml
        |> resolveFinder finder
        |> Result.andThen
            (\found ->
                let
                    err =
                        found
                            |> Event.simulate event
                            |> Event.toResult
                in
                case err of
                    Err e ->
                        Err (failureFromDescription e)

                    Ok msg ->
                        Ok (program.definition.update msg program.model)
            )


failureWithStepsDescription :
    List StepDescription
    -> StepDescription
    -> Failure
    -> Failure
failureWithStepsDescription passedSteps failedStep failure =
    passedSteps
        |> List.map (\s -> "[Passed] " ++ s)
        |> (\l -> l ++ [ "[Failed] " ++ failedStep ++ ":\n" ++ failure.description ])
        |> List.indexedMap (\stepIndex stepDescription -> String.fromInt (stepIndex + 1) ++ ". " ++ stepDescription)
        |> String.join "\n"
        |> (\description -> { failure | description = description })


staticStep : StepDescription -> StaticStep model effect msg -> ProgramState model effect msg -> ProgramState model effect msg
staticStep stepDescription currentStep programState =
    step stepDescription
        (\program ->
            currentStep program
                |> expectationToResult ( program.model, program.pendingEffects )
        )
        programState


step : StepDescription -> Step model effect msg -> ProgramState model effect msg -> ProgramState model effect msg
step stepDescription currentStep programState =
    case programState.result of
        Err _ ->
            programState

        Ok stepsProcessed ->
            let
                result =
                    currentStep programState
            in
            case result of
                Err f ->
                    { programState
                        | result = Err (failureWithStepsDescription stepsProcessed stepDescription f)
                    }

                Ok ( newModel, newEffects ) ->
                    { programState
                        | model = newModel
                        , result = Ok (stepsProcessed ++ [ stepDescription ])
                        , pendingEffects = newEffects
                    }

