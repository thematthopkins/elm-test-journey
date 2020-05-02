module TestJourney exposing
    ( ProgramDefinition
    , click
    , dontSee
    , findSingle
    , finish
    , handleEffect
    , input
    , see
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


type SelectorMultiple
    = SelectorMultiple (List Selector.Selector)


type SelectorSingle
    = SelectorSingle (List Selector.Selector)


type FinderPart
    = FinderPartSingle SelectorSingle
    | FinderPartMultiple SelectorMultiple Int


type alias Finder =
    List FinderPart


findSingle : List Selector.Selector -> Finder
findSingle selector =
    [ FinderPartSingle (SelectorSingle selector) ]


see : Finder -> ProgramState model effect msg -> ProgramState model effect msg
see finder =
    staticStep "see" (seeStep finder)


dontSee : Finder -> ProgramState model effect msg -> ProgramState model effect msg
dontSee finder =
    staticStep "dontSee" (dontSeeStep finder)


resolveFinder : Finder -> Query.Single msg -> Result Failure (Query.Single msg)
resolveFinder finder single =
    List.foldl
        (\finderPart parentResult ->
            parentResult
                |> Result.andThen
                    (\parent ->
                        case finderPart of
                            FinderPartSingle (SelectorSingle selector) ->
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

                            FinderPartMultiple (SelectorMultiple selector) index ->
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
        (Ok single)
        finder


seeStep : Finder -> ProgramState model effect msg -> Expect.Expectation
seeStep finder program =
    program.model
        |> program.definition.view
        |> .body
        |> Html.node "body" []
        |> Query.fromHtml
        |> resolveFinder finder
        |> (\found ->
                case found of
                    Ok _ ->
                        Expect.pass

                    Err e ->
                        failureToExpectation e
           )


dontSeeStep : Finder -> ProgramState model effect msg -> Expect.Expectation
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


dontSeeChildStep : Finder -> FinderPart -> ProgramState model effect msg -> Expect.Expectation
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
                FinderPartSingle (SelectorSingle selector) ->
                    parent
                        |> Query.hasNot selector

                FinderPartMultiple (SelectorMultiple selector) index ->
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


click : Finder -> ProgramState model effect msg -> ProgramState model effect msg
click finder =
    step "click" (simulateEventStep Event.click finder)


input : String -> Finder -> ProgramState model effect msg -> ProgramState model effect msg
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


handleEffectStep : (effect -> Maybe ( msg, Expect.Expectation )) -> Step model effect msg
handleEffectStep fn program =
    case program.pendingEffects of
        effect :: remainingEffects ->
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
    , subscriptions : model -> Sub msg
    , debugToString : effect -> String
    }


type alias ProgramState model effect msg =
    { model : model
    , definition : ProgramDefinition model effect msg
    , pendingEffects : List effect
    , result : Result Failure (List StepDescription)
    }


start : ProgramDefinition model effect msg -> model -> ProgramState model effect msg
start programDefinition model =
    { model = model
    , definition = programDefinition
    , pendingEffects = []
    , result = Ok []
    }


failureWithStepsDescription :
    List StepDescription
    -> StepDescription
    -> Failure
    -> Failure
failureWithStepsDescription passedSteps failedStep failure =
    let
        _ =
            Debug.log "steps: " passedSteps
    in
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
        Err e ->
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
