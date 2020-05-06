module TestJourney exposing
    ( EffectHandlerResult(..)
    , ProgramDefinition
    , blur
    , check
    , click
    , custom
    , dontSee
    , doubleClick
    , expectModel
    , finish
    , focus
    , handleEffect
    , injectMsg
    , input
    , mapModel
    , mouseDown
    , mouseEnter
    , mouseLeave
    , mouseOut
    , mouseOver
    , mouseUp
    , see
    , seeCount
    , seeText
    , start
    , submit
    , uncheck
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
import TestJourney.Internal exposing (Finder(..), FinderPart(..))
import TestJourney.Page as Page


type EffectHandlerResult msg
    = EffectProcessed Expect.Expectation msg
    | EffectUnexpected


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


see : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
see finder =
    staticStep ("see " ++ finderFriendlyName finder.self) (seeStep finder.self)


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
                            FinderPartSingle _ selector ->
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


injectMsg : msg -> ProgramState model effect msg -> ProgramState model effect msg
injectMsg msg =
    step "injectMsg" (\p -> Ok (p.definition.update msg p.model))


mapModel : (model -> model) -> ProgramState model effect msg -> ProgramState model effect msg
mapModel fn =
    step "mapModel" (\p -> Ok ( fn p.model, [] ))


expectModel : (model -> Expect.Expectation) -> ProgramState model effect msg -> ProgramState model effect msg
expectModel fn =
    staticStep "expectModel" (\p -> fn p.model)


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



-- Events


click : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
click finder =
    step ("click " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.click finder.self)


input : String -> Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
input text finder =
    step ("input \"" ++ text ++ "\" on " ++ finderFriendlyName finder.self)
        (simulateEventStep (Event.input text) finder.self)


custom : String -> Json.Encode.Value -> Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
custom eventName value finder =
    step ("custom \"" ++ eventName ++ "\" sent \"" ++ Json.Encode.encode 4 value ++ "\" on " ++ finderFriendlyName finder.self)
        (simulateEventStep (Event.custom eventName value) finder.self)


doubleClick : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
doubleClick finder =
    step ("doubleClick " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.doubleClick finder.self)


mouseDown : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseDown finder =
    step ("mouseDown " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseDown finder.self)


mouseUp : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseUp finder =
    step ("mouseUp " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseUp finder.self)


mouseEnter : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseEnter finder =
    step ("mouseEnter " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseEnter finder.self)


mouseLeave : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseLeave finder =
    step ("mouseLeave " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseLeave finder.self)


mouseOver : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseOver finder =
    step ("mouseOver " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseOver finder.self)


mouseOut : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
mouseOut finder =
    step ("mouseOut " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.mouseOut finder.self)


check : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
check finder =
    step ("check " ++ finderFriendlyName finder.self)
        (simulateEventStep (Event.check True) finder.self)


uncheck : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
uncheck finder =
    step ("uncheck " ++ finderFriendlyName finder.self)
        (simulateEventStep (Event.check False) finder.self)


submit : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
submit finder =
    step ("submit " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.submit finder.self)


blur : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
blur finder =
    step ("blur " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.blur finder.self)


focus : Page.Element children -> ProgramState model effect msg -> ProgramState model effect msg
focus finder =
    step ("focus " ++ finderFriendlyName finder.self)
        (simulateEventStep Event.focus finder.self)
