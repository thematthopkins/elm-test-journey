module TestJourney exposing
    ( startSandbox, SandboxProgram
    , startApplication, ApplicationProgram
    , startElement, ElementProgram
    , startDocument, DocumentProgram
    , startView
    , TestState
    , finish
    , mapModel, expectModel
    , handleEffect, EffectHandlerResult(..)
    , injectMsg
    , blur, check, click, custom, doubleClick, focus, input, mouseDown, mouseEnter, mouseLeave, mouseOut, mouseOver, mouseUp, submit, uncheck
    , see, seeCount, seeText, seeClass, seeAttribute, seeChecked, seeUnchecked, seeDisabled, seeNotDisabled, seeHref, seeSrc, seeValue
    , dontSee, dontSeeClass
    )

{-| Write easy-to-maintain acceptance-like tests.


## Setup

@docs startSandbox, SandboxProgram
@docs startApplication, ApplicationProgram
@docs startElement, ElementProgram
@docs startDocument, DocumentProgram
@docs startView

@docs TestState

@docs finish


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

@docs see, seeCount, seeText, seeClass, seeAttribute, seeChecked, seeUnchecked, seeDisabled, seeNotDisabled, seeHref, seeSrc, seeValue

@docs dontSee, dontSeeClass

-}

import Browser
import Expect
import Html
import Html.Attributes
import Json.Encode
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Test.Runner
import Test.Runner.Failure
import TestJourney.Internal exposing (Finder(..), FinderPart(..))
import TestJourney.Page as Page
import Url


{-| For use in [`startDocument`](#startDocument).

`effectToString` should always be set to `Debug.toString`. Having the caller supply this allows `elm-test-program` to avoid it's use, and be published as a package.

-}
type alias DocumentProgram model msg effect =
    { view : model -> Browser.Document msg
    , update : msg -> model -> ( model, List effect )
    , model : model
    , effectToString : effect -> String
    }


{-| Defines the application under test and starts the test pipeline. Simular to `Browser.document`, but without subscriptions or init, and `List effect` instead of `Cmd`.
-}
startDocument : DocumentProgram model msg effect -> TestState model msg effect
startDocument program =
    start (ProgramDefinitionDocument program)


{-| For use in [`startElement`](#startElement).

`effectToString` should always be set to `Debug.toString`. Having the caller supply this allows `elm-test-program` to avoid it's use, and be published as a package.

-}
type alias ElementProgram model msg effect =
    { view : model -> Html.Html msg
    , update : msg -> model -> ( model, List effect )
    , model : model
    , effectToString : effect -> String
    }


{-| Defines the application under test and starts the test pipeline. Simular to `Browser.element`, but without subscriptions or init, and `List effect` instead of `Cmd`.

This is also useful if you want to want to limite the system under test to a subsection of your application, like a single page of an application, or maybe a single widget within your application.

-}
startElement : ElementProgram model msg effect -> TestState model msg effect
startElement program =
    start (ProgramDefinitionElement program)


{-| Defines the application under test as a static Html.Html. Useful if you want to test a view function in isolation.
-}
startView : Html.Html msg -> TestState model msg effect
startView html =
    start (ProgramDefinitionHtml html)


{-| For use in [`startApplication`](#startApplication).

`effectToString` should always be set to `Debug.toString`. Having the caller supply this allows `elm-test-program` to avoid it's use, and be published as a package.

-}
type alias ApplicationProgram model msg effect =
    { view : model -> Browser.Document msg
    , update : msg -> model -> ( model, List effect )
    , model : model
    , onUrlRequest : Browser.UrlRequest -> msg
    , onUrlChange : Url.Url -> msg
    , effectToString : effect -> String
    }


{-| Defines the application under test and starts the test pipeline. Simular to `Browser.application`, but without subscriptions or init, and `List effect` instead of `Cmd`.
-}
startApplication : ApplicationProgram model msg effect -> TestState model msg effect
startApplication program =
    start (ProgramDefinitionApplication program)


{-| For use in [`startSandbox`](#startSandbox).

`effectToString` should always be set to `Debug.toString`. Having the caller supply this allows `elm-test-program` to avoid it's use, and be published as a package.

-}
type alias SandboxProgram model msg =
    { view : model -> Html.Html msg
    , update : msg -> model -> model
    , model : model
    }


{-| Defines the application under test and starts the test pipeline. Simular to `Browser.sandbox`.
-}
startSandbox : SandboxProgram model msg -> TestState model msg effect
startSandbox program =
    start (ProgramDefinitionSandbox program)


type ProgramDefinition model msg effect
    = ProgramDefinitionApplication (ApplicationProgram model msg effect)
    | ProgramDefinitionSandbox (SandboxProgram model msg)
    | ProgramDefinitionElement (ElementProgram model msg effect)
    | ProgramDefinitionDocument (DocumentProgram model msg effect)
    | ProgramDefinitionHtml (Html.Html msg)


start : ProgramDefinition model msg effect -> TestState model msg effect
start program =
    TestState
        { program = program
        , pendingEffects = []
        , result = Ok []
        }


programEffectToString : ProgramDefinition model msg effect -> Maybe (effect -> String)
programEffectToString program =
    case program of
        ProgramDefinitionApplication p ->
            Just p.effectToString

        ProgramDefinitionSandbox _ ->
            Nothing

        ProgramDefinitionElement p ->
            Just p.effectToString

        ProgramDefinitionDocument p ->
            Just p.effectToString

        ProgramDefinitionHtml _ ->
            Nothing


{-| Call at the end of your test. Automatically add an expectations that there are no
pending effects left to process, since that is usually inadvertant in practice.
-}
finish : TestState model msg effect -> Expect.Expectation
finish (TestState testState) =
    case testState.result of
        Err e ->
            failureToExpectation e

        Ok stepsProcessed ->
            if stepsProcessed == [] then
                Expect.fail "Empty test failed - no steps or expectations provided"

            else if testState.pendingEffects /= [] then
                case programEffectToString testState.program of
                    Nothing ->
                        Expect.fail "Failed with pending effects and no way to print them"

                    Just effectToString ->
                        failureWithStepsDescription
                            stepsProcessed
                            ("test finished with "
                                ++ String.fromInt (List.length testState.pendingEffects)
                                ++ " pending expectations that were left unhandled"
                            )
                            (failureFromDescription
                                (testState.pendingEffects
                                    |> List.map effectToString
                                    |> List.map (\s -> "    " ++ s)
                                    |> String.join "\n"
                                )
                            )
                            |> failureToExpectation

            else
                Expect.pass


{-| Directly transform the underlying model. This can be useful for altering your [`model`](#ProgramDefinition) supplied in [`ProgramDefinition`](#ProgramDefinition). This should generally be avoided in the body of your tests.
-}
mapModel : (model -> model) -> TestState model msg effect -> TestState model msg effect
mapModel fn =
    step "mapModel"
        (\program effects ->
            let
                op =
                    \constructor programWithModel ->
                        let
                            newModel =
                                fn programWithModel.model
                        in
                        Ok ( constructor { programWithModel | model = newModel }, effects )
            in
            case program of
                ProgramDefinitionApplication p ->
                    op ProgramDefinitionApplication p

                ProgramDefinitionSandbox p ->
                    op ProgramDefinitionSandbox p

                ProgramDefinitionElement p ->
                    op ProgramDefinitionElement p

                ProgramDefinitionDocument p ->
                    op ProgramDefinitionDocument p

                ProgramDefinitionHtml _ ->
                    Err (failureFromDescription "Operation not permitted for startView.  Try startSandbox to include a model.")
        )


{-| Directly run an expectation on the current model of the application under tests. Useful when there are changes that aren't represented in the UI (e.g. I want to make sure I get a new session id after logging in).
-}
expectModel : (model -> Expect.Expectation) -> TestState model msg effect -> TestState model msg effect
expectModel fn =
    stepExpectProgram "expectModel"
        (\program ->
            let
                op =
                    \programWithModel ->
                        fn programWithModel.model
            in
            case program of
                ProgramDefinitionApplication p ->
                    op p

                ProgramDefinitionSandbox p ->
                    op p

                ProgramDefinitionElement p ->
                    op p

                ProgramDefinitionDocument p ->
                    op p

                ProgramDefinitionHtml _ ->
                    Expect.fail "Operation not permitted for startView.  Try startSandbox to include a model."
        )


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
handleEffect : (effect -> EffectHandlerResult msg) -> TestState model msg effect -> TestState model msg effect
handleEffect fn (TestState testState) =
    case testState.pendingEffects of
        [] ->
            stepFail "handleEffect" "attempted to handle effect when no effects were generated or no pending effects remain" (TestState testState)

        nextEffect :: remainingEffects ->
            case programEffectToString testState.program of
                Nothing ->
                    stepFail "handleEffect" "Operation not permitted for startSandbox.  Try startElement for programs with effects." (TestState testState)

                Just effectToString ->
                    step ("handleEffect " ++ effectToString nextEffect)
                        (\prog _ ->
                            case fn nextEffect of
                                EffectProcessed expect msg ->
                                    let
                                        failure =
                                            expectationToFailure expect
                                    in
                                    case failure of
                                        Just f ->
                                            Err f

                                        Nothing ->
                                            update msg prog remainingEffects

                                EffectSeen expect ->
                                    let
                                        failure =
                                            expectationToFailure expect
                                    in
                                    case failure of
                                        Just f ->
                                            Err f

                                        Nothing ->
                                            Ok ( prog, remainingEffects )

                                EffectUnexpected ->
                                    Err (failureFromDescription ("Unhandled effect: " ++ effectToString nextEffect))
                        )
                        (TestState testState)


{-| Used by [`handleEffect`](#handleEffect) to create expectations around the next Effect, and optionally inject a message simulating the result of the effect.

To play the part of the server in an http request, use `EffectProcessed`:

        J.startDocument program
            |> J.click page.addItemButton
            |> J.handleEffect
                (\effect ->
                    case effect of
                        TodoExample.EffectAddItem msg input ->
                            J.EffectProcessed
                                (Expect.equal
                                    "myNewItem"
                                    input
                                )
                                (msg
                                    (Ok ())
                                )

                        _ ->
                            J.EffectUnexpected
                )
            |> J.finish

To perform expectations on an effect without generating an msg (useful for testing `port`s, which are fire-and-forget):

        J.startDocument program
            |> J.click page.addItemButton
            |> J.handleEffect
                (\effect ->
                    case effect of
                        TodoExample.EffectPopJsAlert alertText ->
                            J.EffectSeen
                                (Expect.equal
                                    "Item already exists"
                                    alertText
                                )

                        _ ->
                            J.EffectUnexpected
                )
            |> J.finish

-}
type EffectHandlerResult msg
    = EffectProcessed Expect.Expectation msg
    | EffectSeen Expect.Expectation
    | EffectUnexpected


{-| The state of the elm program under test, and its pending effects. Create using one of the `start*` functions.
-}
type TestState model msg effect
    = TestState (TestStateRecord model msg effect)


type alias TestStateRecord model msg effect =
    { program : ProgramDefinition model msg effect
    , pendingEffects : List effect
    , result : Result Failure (List StepDescription)
    }


update :
    msg
    -> ProgramDefinition model msg effect
    -> List effect
    -> Result Failure ( ProgramDefinition model msg effect, List effect )
update msg program effects =
    let
        effectOp =
            \constructor programWithUpdate ->
                let
                    ( newModel, addedEffects ) =
                        programWithUpdate.update msg programWithUpdate.model
                in
                Ok ( constructor { programWithUpdate | model = newModel }, effects ++ addedEffects )

        noEffectOp =
            \constructor programWithUpdate ->
                let
                    newModel =
                        programWithUpdate.update msg programWithUpdate.model
                in
                Ok ( constructor { programWithUpdate | model = newModel }, effects )
    in
    case program of
        ProgramDefinitionApplication p ->
            effectOp ProgramDefinitionApplication p

        ProgramDefinitionSandbox p ->
            noEffectOp ProgramDefinitionSandbox p

        ProgramDefinitionElement p ->
            effectOp ProgramDefinitionElement p

        ProgramDefinitionDocument p ->
            effectOp ProgramDefinitionDocument p

        ProgramDefinitionHtml _ ->
            Err (failureFromDescription "invalid opration on startView.  Try startElement so you have an update function.")


{-| Sends the supplied `msg` into appliication under test. This is how `ports` and `Subscriptions` should be simulated.
-}
injectMsg : msg -> TestState model msg effect -> TestState model msg effect
injectMsg msg =
    step "injectMsg" (update msg)



-- Html Events


{-| Ensures the target element exists exactly once, simulates a click event, and processes the generated message.
-}
click : Page.Element children -> TestState model msg effect -> TestState model msg effect
click finder =
    stepSimulateEvent ("click " ++ finderFriendlyName finder.self) Event.click finder.self


{-| Ensures the target element exists exactly once, simulates a input event w/ the given text, and processes the generated message.
-}
input : String -> Page.Element children -> TestState model msg effect -> TestState model msg effect
input text finder =
    stepSimulateEvent ("input \"" ++ text ++ "\" on " ++ finderFriendlyName finder.self) (Event.input text) finder.self


{-| Ensures the target element exists exactly once, simulates a custom, and processes the generated message.

                        See (see [Test.Html.Event.custom](https://package.elm-lang.org/packages/elm-explorations/test/latest/Test-Html-Event#custom))

-}
custom : String -> Json.Encode.Value -> Page.Element children -> TestState model msg effect -> TestState model msg effect
custom eventName value finder =
    stepSimulateEvent ("custom \"" ++ eventName ++ "\" sent \"" ++ Json.Encode.encode 4 value ++ "\" on " ++ finderFriendlyName finder.self) (Event.custom eventName value) finder.self


{-| Ensures the target element exists exactly once, simulates a double-click event, and processes the generated message.
-}
doubleClick : Page.Element children -> TestState model msg effect -> TestState model msg effect
doubleClick finder =
    stepSimulateEvent ("doubleClick " ++ finderFriendlyName finder.self) Event.doubleClick finder.self


{-| Ensures the target element exists exactly once, simulates a mouseDown event, and processes the generated message.
-}
mouseDown : Page.Element children -> TestState model msg effect -> TestState model msg effect
mouseDown finder =
    stepSimulateEvent ("mouseDown " ++ finderFriendlyName finder.self) Event.mouseDown finder.self


{-| Ensures the target element exists exactly once, simulates a mouseUp event, and processes the generated message.
-}
mouseUp : Page.Element children -> TestState model msg effect -> TestState model msg effect
mouseUp finder =
    stepSimulateEvent ("mouseUp " ++ finderFriendlyName finder.self) Event.mouseUp finder.self


{-| Ensures the target element exists exactly once, simulates a mouseEnter event, and processes the generated message.
-}
mouseEnter : Page.Element children -> TestState model msg effect -> TestState model msg effect
mouseEnter finder =
    stepSimulateEvent ("mouseEnter " ++ finderFriendlyName finder.self) Event.mouseEnter finder.self


{-| Ensures the target element exists exactly once, simulates a mouseLeave event, and processes the generated message.
-}
mouseLeave : Page.Element children -> TestState model msg effect -> TestState model msg effect
mouseLeave finder =
    stepSimulateEvent ("mouseLeave " ++ finderFriendlyName finder.self) Event.mouseLeave finder.self


{-| Ensures the target element exists exactly once, simulates a mouseOver event, and processes the generated message.
-}
mouseOver : Page.Element children -> TestState model msg effect -> TestState model msg effect
mouseOver finder =
    stepSimulateEvent ("mouseOver " ++ finderFriendlyName finder.self) Event.mouseOver finder.self


{-| Ensures the target element exists exactly once, simulates a mouseOut event, and processes the generated message.
-}
mouseOut : Page.Element children -> TestState model msg effect -> TestState model msg effect
mouseOut finder =
    stepSimulateEvent ("mouseOut " ++ finderFriendlyName finder.self) Event.mouseOut finder.self


{-| Ensures the target element exists exactly once, simulates a check true event, and processes the generated message.
-}
check : Page.Element children -> TestState model msg effect -> TestState model msg effect
check finder =
    stepSimulateEvent ("check " ++ finderFriendlyName finder.self) (Event.check True) finder.self


{-| Ensures the target element exists exactly once, simulates an check false event, and processes the generated message.
-}
uncheck : Page.Element children -> TestState model msg effect -> TestState model msg effect
uncheck finder =
    stepSimulateEvent ("uncheck " ++ finderFriendlyName finder.self) (Event.check False) finder.self


{-| Ensures the target element exists exactly once, simulates a submit event, and processes the generated message.
-}
submit : Page.Element children -> TestState model msg effect -> TestState model msg effect
submit finder =
    stepSimulateEvent ("submit " ++ finderFriendlyName finder.self) Event.submit finder.self


{-| Ensures the target element exists exactly once, simulates a blur event, and processes the generated message.
-}
blur : Page.Element children -> TestState model msg effect -> TestState model msg effect
blur finder =
    stepSimulateEvent ("blur " ++ finderFriendlyName finder.self) Event.blur finder.self


{-| Ensures the target element exists exactly once, simulates a focus event, and processes the generated message.
-}
focus : Page.Element children -> TestState model msg effect -> TestState model msg effect
focus finder =
    stepSimulateEvent ("focus " ++ finderFriendlyName finder.self) Event.focus finder.self



-- Expectations


{-| Expect exactly a given number of matching elements to exist.
-}
seeCount : Int -> (Int -> Page.Element children) -> TestState model msg effect -> TestState model msg effect
seeCount expectedCount finderFn =
    let
        (Finder finder) =
            finderFn 0
                |> .self
    in
    case List.reverse finder of
        [] ->
            stepFail "seeCount" "Invalid finder for seeCount"

        lastPart :: rest ->
            let
                parentFinder =
                    List.reverse rest
            in
            case lastPart of
                FinderPartMultiple name selector _ ->
                    stepExpectView
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
                        (\html ->
                            html
                                |> Query.fromHtml
                                |> resolveFinder (Finder parentFinder)
                                |> (\found ->
                                        case found of
                                            Ok parent ->
                                                parent
                                                    |> Query.findAll selector
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

                                            Err err ->
                                                failureToExpectation err
                                   )
                        )

                _ ->
                    stepFail "seeCount" "Invalid single finder for seeCount"


{-| Expect a given element to exist exactly once.
-}
see : Page.Element children -> TestState model msg effect -> TestState model msg effect
see finder =
    stepSee ("see " ++ finderFriendlyName finder.self) finder.self


stepSeeProperty : String -> List Selector.Selector -> Page.Element children -> TestState model msg effect -> TestState model msg effect
stepSeeProperty label selector element =
    let
        (Finder finder) =
            element.self
    in
    stepSee (label ++ " in " ++ finderFriendlyName (Finder finder))
        (Finder
            (finder ++ [ FinderPartSingle "" selector ])
        )


{-| Expect text within itself or a descendant.
-}
seeText : String -> Page.Element children -> TestState model msg effect -> TestState model msg effect
seeText expect =
    stepSeeProperty ("seeText \"" ++ expect ++ "\"") [ Selector.text expect ]


{-| Expect a class to exist on the element or a descendant.
-}
seeClass : String -> Page.Element children -> TestState model msg effect -> TestState model msg effect
seeClass expect =
    stepSeeProperty ("seeClass \"" ++ expect ++ "\"") [ Selector.class expect ]


{-| Expect a value attribute to exist on the element or a descendant.
-}
seeValue : String -> Page.Element children -> TestState model msg effect -> TestState model msg effect
seeValue expect =
    stepSeeProperty ("seeValue \"" ++ expect ++ "\"") [ Selector.attribute (Html.Attributes.value expect) ]


{-| Expect a class to not exist on the element and it's descendants.
-}
dontSeeClass : String -> Page.Element children -> TestState model msg effect -> TestState model msg effect
dontSeeClass expect =
    stepDontSeeProperty ("dontSeeClass \"" ++ expect ++ "\"") [ Selector.class expect ]


{-| Expect an attribute attribute to exist on the element or a descendant.
-}
seeAttribute : String -> String -> Page.Element children -> TestState model msg effect -> TestState model msg effect
seeAttribute attrName expect =
    stepSeeProperty ("seeAttribute \"[" ++ attrName ++ "=" ++ expect ++ "]\"") [ Selector.attribute (Html.Attributes.attribute attrName expect) ]


{-| Expect an href attribute attribute to exist on the element or a descendant.
-}
seeHref : String -> Page.Element children -> TestState model msg effect -> TestState model msg effect
seeHref expect =
    stepSeeProperty ("seeHref \"" ++ expect ++ "\"") [ Selector.attribute (Html.Attributes.href expect) ]


{-| Expect a src attribute to exist on the element or a descendant.
-}
seeSrc : String -> Page.Element children -> TestState model msg effect -> TestState model msg effect
seeSrc expect =
    stepSeeProperty ("seeSrc \"" ++ expect ++ "\"") [ Selector.attribute (Html.Attributes.src expect) ]


{-| Expect a disabled attribute to exist on the element or a descendant.
-}
seeDisabled : Page.Element children -> TestState model msg effect -> TestState model msg effect
seeDisabled =
    stepSeeProperty "seeDisabled" [ Selector.disabled True ]


{-| Expect a disabled attribute be false on an element or a descendant.
-}
seeNotDisabled : Page.Element children -> TestState model msg effect -> TestState model msg effect
seeNotDisabled =
    stepSeeProperty "seeNotDisabled" [ Selector.disabled False ]


{-| Expect the element or a descendant to be checked.
-}
seeChecked : Page.Element children -> TestState model msg effect -> TestState model msg effect
seeChecked =
    stepSeeProperty "seeChecked" [ Selector.checked True ]


{-| Expect the element or a descendant to be unchecked.
-}
seeUnchecked : Page.Element children -> TestState model msg effect -> TestState model msg effect
seeUnchecked =
    stepSeeProperty "seeUnchecked" [ Selector.checked False ]


{-| Expect a given element to not exist. If the target is a sequence of page finders (e.g. `parent.child.grandchild`), `dontSee` only passes if
`parent`, and `child` exist, but `grandchild` does not. Otherwise, it's too easy for `dontSee` to inadvertantly pass.
-}
dontSee : Page.Element children -> TestState model msg effect -> TestState model msg effect
dontSee finder =
    stepDontSee ("dontSee " ++ finderFriendlyName finder.self) finder.self


resolveFinder : Page.Finder -> Query.Single msg -> Result Failure (Query.Single msg)
resolveFinder (Finder finder) query =
    List.foldl
        (\finderPart parentResult ->
            parentResult
                |> Result.andThen
                    (\parent ->
                        case finderPart of
                            FinderPartSingle _ [] ->
                                -- Empty selectors are frequently used to represent the root element.
                                -- Skip finding, since we know the parent already has what we're looking for, and
                                -- doing an extra find will display this extra noop in expectation failures.
                                parentResult

                            FinderPartSingle _ selector ->
                                let
                                    failure =
                                        parent
                                            |> Query.has selector
                                            |> expectationToFailure
                                            |> (\notFoundOnSelfFailure ->
                                                    case notFoundOnSelfFailure of
                                                        Just _ ->
                                                            parent
                                                                |> Query.find selector
                                                                |> Query.has []
                                                                |> expectationToFailure

                                                        Nothing ->
                                                            Nothing
                                               )
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
                                        let
                                            expectCount =
                                                parent
                                                    |> Query.findAll selector
                                                    |> Query.count
                                                        (\c ->
                                                            if c <= index then
                                                                Expect.fail ("Expected to find at least " ++ String.fromInt (index + 1) ++ " matches but found " ++ String.fromInt c)

                                                            else
                                                                Expect.pass
                                                        )
                                                    |> expectationToFailure
                                        in
                                        case expectCount of
                                            Just f ->
                                                Err f

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


stepSee : StepDescription -> Page.Finder -> TestState model msg effect -> TestState model msg effect
stepSee description finder =
    stepExpectView description
        (\html ->
            html
                |> Query.fromHtml
                |> resolveFinder finder
                |> resultToExpectation
        )


stepDontSeeProperty : String -> List Selector.Selector -> Page.Element children -> TestState model msg effect -> TestState model msg effect
stepDontSeeProperty label selector element =
    let
        (Finder finder) =
            element.self
    in
    stepDontSee (label ++ " in " ++ finderFriendlyName (Finder finder))
        (Finder
            (finder ++ [ FinderPartSingle "" selector ])
        )


stepDontSee : StepDescription -> Page.Finder -> TestState model msg effect -> TestState model msg effect
stepDontSee description (Finder finder) =
    finder
        |> List.reverse
        |> (\reversed ->
                case reversed of
                    [] ->
                        stepFail description "No selector provided"

                    lastFinder :: rest ->
                        let
                            parentFinder =
                                List.reverse rest
                        in
                        stepDontSeeChild description (Finder parentFinder) lastFinder
           )


stepDontSeeChild : StepDescription -> Page.Finder -> FinderPart -> TestState model msg effect -> TestState model msg effect
stepDontSeeChild description parentFinder childFinder =
    stepExpectView description
        (\html ->
            let
                parentResult =
                    html
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
        )


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
        |> (\s ->
                if s == "" then
                    "<root element>"

                else
                    s
           )


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


view : ProgramDefinition model msg effect -> Html.Html msg
view program =
    case program of
        ProgramDefinitionApplication p ->
            p.model
                |> p.view
                |> .body
                |> Html.node "body" []

        ProgramDefinitionSandbox p ->
            p.view p.model

        ProgramDefinitionElement p ->
            p.view p.model

        ProgramDefinitionDocument p ->
            p.model
                |> p.view
                |> .body
                |> Html.node "body" []

        ProgramDefinitionHtml p ->
            p


stepSimulateEvent :
    StepDescription
    -> ( String, Json.Encode.Value )
    -> Finder
    -> TestState model msg effect
    -> TestState model msg effect
stepSimulateEvent description event finder =
    step description
        (\program effects ->
            program
                |> view
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
                                update msg program effects
                    )
        )


failureWithStepsDescription :
    List StepDescription
    -> StepDescription
    -> Failure
    -> Failure
failureWithStepsDescription passedSteps failedStep failure =
    passedSteps
        |> List.map (\s -> "✓ " ++ s)
        |> (\l -> l ++ [ "✗ " ++ failedStep ++ ":\n" ++ Test.Runner.Failure.format failure.description failure.reason ])
        |> String.join "\n"
        |> failureFromDescription


stepExpectProgram :
    StepDescription
    -> (ProgramDefinition model msg effect -> Expect.Expectation)
    -> TestState model msg effect
    -> TestState model msg effect
stepExpectProgram description fn =
    step description
        (\program effects ->
            fn program
                |> expectationToResult ( program, effects )
        )


stepExpectView :
    StepDescription
    -> (Html.Html msg -> Expect.Expectation)
    -> TestState model msg effect
    -> TestState model msg effect
stepExpectView description fn =
    step description
        (\program effects ->
            program
                |> view
                |> fn
                |> expectationToResult ( program, effects )
        )


stepFail :
    StepDescription
    -> String
    -> TestState model msg effect
    -> TestState model msg effect
stepFail description reason =
    step description (\_ _ -> Err (failureFromDescription reason))


step :
    StepDescription
    -> (ProgramDefinition model msg effect -> List effect -> Result Failure ( ProgramDefinition model msg effect, List effect ))
    -> TestState model msg effect
    -> TestState model msg effect
step stepDescription fn (TestState testState) =
    case testState.result of
        Err _ ->
            TestState testState

        Ok stepsProcessed ->
            let
                result =
                    fn testState.program testState.pendingEffects
            in
            case result of
                Err f ->
                    TestState
                        { testState
                            | result = Err (failureWithStepsDescription stepsProcessed stepDescription f)
                        }

                Ok ( newProgram, newEffects ) ->
                    TestState
                        { testState
                            | program = newProgram
                            , result = Ok (stepsProcessed ++ [ stepDescription ])
                            , pendingEffects = newEffects
                        }
