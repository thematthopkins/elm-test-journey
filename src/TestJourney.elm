module TestJourney exposing
    ( ProgramDefinition
    , click
    , dontSee
    , finish
    , handleEffect
    , input
    , mapModel
    , see
    , seeCount
    , seeText
    , start
    )

import Browser
import Expect
import Html
import Json.Encode
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner
import Test.Runner.Failure
import TestJourney.Page as Page


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


seeCount : Int -> (Int -> { a | self : Page.Finder }) -> ProgramState model effect msg -> ProgramState model effect msg
seeCount expectedCount finderFn model =
    let
        finder =
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
                Page.FinderPartMultiple name (Page.SelectorMultiple selector) _ ->
                    staticStep
                        ("seeCount "
                            ++ String.fromInt expectedCount
                            ++ " of "
                            ++ Page.finderFriendlyName
                                (parentFinder
                                    ++ [ Page.FinderPartSingle name (Page.SelectorSingle selector) ]
                                )
                        )
                        (seeCountStep expectedCount parentFinder (Page.SelectorMultiple selector))
                        model

                _ ->
                    { model | result = Err (failureFromDescription "Invalid single finder for seeCount") }


seeCountStep : Int -> Page.Finder -> Page.SelectorMultiple -> ProgramState model effect msg -> Expect.Expectation
seeCountStep expectedCount parentFinder (Page.SelectorMultiple lastFinderPart) program =
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


see : Page.Finder -> ProgramState model effect msg -> ProgramState model effect msg
see finder =
    staticStep ("see " ++ Page.finderFriendlyName finder) (seeStep finder)


seeText : String -> Page.Finder -> ProgramState model effect msg -> ProgramState model effect msg
seeText expectText finder =
    staticStep ("seeText \"" ++ expectText ++ "\" at " ++ Page.finderFriendlyName finder)
        (seeStep
            (finder ++ [ Page.FinderPartSingle "" (Page.SelectorSingle [ Selector.text expectText ]) ])
        )


dontSee : Page.Finder -> ProgramState model effect msg -> ProgramState model effect msg
dontSee finder =
    staticStep ("dontSee " ++ Page.finderFriendlyName finder) (dontSeeStep finder)


resolveFinder : Page.Finder -> Query.Single msg -> Result Failure (Query.Single msg)
resolveFinder finder query =
    List.foldl
        (\finderPart parentResult ->
            parentResult
                |> Result.andThen
                    (\parent ->
                        case finderPart of
                            Page.FinderPartSingle _ (Page.SelectorSingle selector) ->
                                let
                                    failure =
                                        parent
                                            |> Query.has selector
                                            |> expectationToFailure
                                in
                                case failure of
                                    Nothing ->
                                        Ok (Query.find selector parent)

                                    Just f ->
                                        Err f

                            Page.FinderPartMultiple _ (Page.SelectorMultiple selector) index ->
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
dontSeeStep finder program =
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
                        dontSeeChildStep parentFinder lastFinder program
           )


dontSeeChildStep : Page.Finder -> Page.FinderPart -> ProgramState model effect msg -> Expect.Expectation
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
                Page.FinderPartSingle _ (Page.SelectorSingle selector) ->
                    parent
                        |> Query.hasNot selector

                Page.FinderPartMultiple _ (Page.SelectorMultiple selector) index ->
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


click : Page.Finder -> ProgramState model effect msg -> ProgramState model effect msg
click finder =
    step ("click " ++ Page.finderFriendlyName finder) (simulateEventStep Event.click finder)


input : String -> Page.Finder -> ProgramState model effect msg -> ProgramState model effect msg
input text finder =
    step ("input \"" ++ text ++ "\"") (simulateEventStep (Event.input text) finder)


handleEffect : (effect -> Maybe ( msg, Expect.Expectation )) -> ProgramState model effect msg -> ProgramState model effect msg
handleEffect fn programState =
    let
        nextEffect =
            programState.pendingEffects
                |> List.head
                |> Maybe.map (\e -> "(" ++ programState.definition.debugToString e ++ ")")
                |> Maybe.withDefault ""
    in
    step ("handleEffect " ++ nextEffect) (handleEffectStep fn) programState


type alias StepDescription =
    String


type alias Step model effect msg =
    ProgramState model effect msg -> Result Failure ( model, List effect )


type alias StaticStep model effect msg =
    ProgramState model effect msg -> Expect.Expectation


simulateEventStep : ( String, Json.Encode.Value ) -> Page.Finder -> Step model effect msg
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


handleEffectStep : (effect -> Maybe ( msg, Expect.Expectation )) -> Step model effect msg
handleEffectStep fn program =
    case program.pendingEffects of
        effect :: _ ->
            let
                result =
                    fn effect
            in
            case result of
                Just ( msg, expect ) ->
                    let
                        failure =
                            expectationToFailure expect
                    in
                    case failure of
                        Just f ->
                            Err f

                        Nothing ->
                            Ok (program.definition.update msg program.model)

                Nothing ->
                    Err (failureFromDescription ("Unhandled effect: " ++ program.definition.debugToString effect))

        [] ->
            Err (failureFromDescription "attempted to handle effect when no effects were generated")


type alias ProgramDefinition model effect msg =
    { view : model -> Browser.Document msg
    , update : msg -> model -> ( model, List effect )
    , initialModel : model
    , subscriptions : model -> Sub msg
    , debugToString : effect -> String
    }


type alias ProgramState model effect msg =
    { model : model
    , definition : ProgramDefinition model effect msg
    , pendingEffects : List effect
    , result : Result Failure (List StepDescription)
    }


start : ProgramDefinition model effect msg -> ProgramState model effect msg
start programDefinition =
    { model = programDefinition.initialModel
    , definition = programDefinition
    , pendingEffects = []
    , result = Ok []
    }


mapModel : (model -> model) -> ProgramState model effect msg -> ProgramState model effect msg
mapModel fn program =
    { program | model = fn program.model }


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
