module TestJourneyTest exposing (suite)

import Expect
import Html exposing (..)
import Html.Attributes exposing (..)
import Test exposing (..)
import Test.Runner
import Test.Runner.Failure
import TestJourney as J
import TestJourney.Page as Page


type alias Failure =
    { given : Maybe String
    , description : String
    , reason : Test.Runner.Failure.Reason
    }


journeyFailure : Html msg -> Page.Element children -> (Page.Element children -> J.TestState model msg effect -> J.TestState model msg effect) -> Maybe Failure
journeyFailure view element step =
    J.startView view
        |> step element
        |> J.finish
        |> Test.Runner.getFailureReason


expectFailureDescription : List String -> Maybe Failure -> Expect.Expectation
expectFailureDescription description failure =
    failure
        |> Maybe.map
            (\f ->
                Expect.equal (f.description |> String.split "\n") description
            )
        |> Maybe.withDefault (Expect.fail "Expected failure")


suite : Test
suite =
    describe "Test Journey should have nice error messages on failures"
        (let
            subjectView =
                div
                    []
                    [ div [ attribute "data-test" "my-child", class "class-on-my-child-0" ]
                        [ div [ class "class-within-my-child-0" ]
                            []
                        ]
                    , div [ attribute "data-test" "my-child" ]
                        []
                    ]

            subjectPage =
                Page.root []
                    (\root ->
                        { self = root
                        , children =
                            Page.multipleRecordTestAttr root
                                "my-child"
                                (\child ->
                                    { self = child
                                    }
                                )
                        }
                    )
         in
         [ test "Dont See Child - pass" <|
            \_ ->
                journeyFailure
                    subjectView
                    (subjectPage.children 2)
                    J.dontSee
                    |> Expect.equal Nothing
         , test "Dont See Child - fail" <|
            \_ ->
                journeyFailure
                    subjectView
                    (subjectPage.children 1)
                    J.dontSee
                    |> expectFailureDescription
                        [ "✗ dontSee .my-child[1]:"
                        , "found at least 2matching selector"
                        , "▼ Query.fromHtml"
                        , ""
                        , "    <div>"
                        , "        <div class=\"class-on-my-child-0\" data-test=\"my-child\">"
                        , "            <div class=\"class-within-my-child-0\">"
                        , "            </div>"
                        , "        </div>"
                        , "        <div data-test=\"my-child\">"
                        , "        </div>"
                        , "    </div>"
                        , ""
                        , ""
                        , "▼ Query.hasNot [ attribute \"data-test\" \"my-child\" ]"
                        , ""
                        , "✗ has not attribute \"data-test\" \"my-child\""
                        ]
         , test "Dont See class within child - pass" <|
            \_ ->
                journeyFailure
                    subjectView
                    (subjectPage.children 1)
                    (J.dontSeeClass "class-within-my-child-0")
                    |> Expect.equal Nothing
         , test "Dont See class within child - fail" <|
            \_ ->
                journeyFailure
                    subjectView
                    (subjectPage.children 0)
                    (J.dontSeeClass "class-within-my-child-0")
                    |> expectFailureDescription
                        [ "✗ dontSeeClass \"class-within-my-child-0\" in .my-child[0]:"
                        , "▼ Query.fromHtml"
                        , ""
                        , "    <div>"
                        , "        <div class=\"class-on-my-child-0\" data-test=\"my-child\">"
                        , "            <div class=\"class-within-my-child-0\">"
                        , "            </div>"
                        , "        </div>"
                        , "        <div data-test=\"my-child\">"
                        , "        </div>"
                        , "    </div>"
                        , ""
                        , ""
                        , "▼ Query.findAll [ attribute \"data-test\" \"my-child\" ]"
                        , ""
                        , "    1)  <div class=\"class-on-my-child-0\" data-test=\"my-child\">"
                        , "            <div class=\"class-within-my-child-0\">"
                        , "            </div>"
                        , "        </div>"
                        , ""
                        , "    2)  <div data-test=\"my-child\">"
                        , "        </div>"
                        , ""
                        , ""
                        , "▼ Query.index 0"
                        , ""
                        , "    1)  <div class=\"class-on-my-child-0\" data-test=\"my-child\">"
                        , "            <div class=\"class-within-my-child-0\">"
                        , "            </div>"
                        , "        </div>"
                        , ""
                        , ""
                        , "▼ Query.hasNot [ class \"class-within-my-child-0\" ]"
                        , ""
                        , "✗ has not class \"class-within-my-child-0\""
                        ]
         , test "See child - pass" <|
            \_ ->
                journeyFailure
                    subjectView
                    (subjectPage.children 1)
                    J.see
                    |> Expect.equal Nothing
         , test "See child - fail" <|
            \_ ->
                journeyFailure
                    subjectView
                    (subjectPage.children 2)
                    J.see
                    |> expectFailureDescription
                        [ "✗ see .my-child[2]:"
                        , "▼ Query.fromHtml"
                        , ""
                        , "    <div>"
                        , "        <div class=\"class-on-my-child-0\" data-test=\"my-child\">"
                        , "            <div class=\"class-within-my-child-0\">"
                        , "            </div>"
                        , "        </div>"
                        , "        <div data-test=\"my-child\">"
                        , "        </div>"
                        , "    </div>"
                        , ""
                        , ""
                        , "▼ Query.findAll [ attribute \"data-test\" \"my-child\" ]"
                        , ""
                        , "    1)  <div class=\"class-on-my-child-0\" data-test=\"my-child\">"
                        , "            <div class=\"class-within-my-child-0\">"
                        , "            </div>"
                        , "        </div>"
                        , ""
                        , "    2)  <div data-test=\"my-child\">"
                        , "        </div>"
                        , ""
                        , ""
                        , "▼ Query.count"
                        , ""
                        , "Expected to find at least 3 matches but found 2"
                        ]
         , test "See class within child - pass" <|
            \_ ->
                journeyFailure
                    subjectView
                    (subjectPage.children 0)
                    (J.seeClass "class-within-my-child-0")
                    |> Expect.equal Nothing
         , test "See class on child - pass" <|
            \_ ->
                journeyFailure
                    subjectView
                    (subjectPage.children 0)
                    (J.seeClass "class-on-my-child-0")
                    |> Expect.equal Nothing
         , test "See class within child - fail" <|
            \_ ->
                journeyFailure
                    subjectView
                    (subjectPage.children 1)
                    (J.seeClass "class-within-my-child-0")
                    |> expectFailureDescription
                        [ "✗ seeClass \"class-within-my-child-0\" in .my-child[1]:"
                        , "▼ Query.fromHtml"
                        , ""
                        , "    <div>"
                        , "        <div class=\"class-on-my-child-0\" data-test=\"my-child\">"
                        , "            <div class=\"class-within-my-child-0\">"
                        , "            </div>"
                        , "        </div>"
                        , "        <div data-test=\"my-child\">"
                        , "        </div>"
                        , "    </div>"
                        , ""
                        , ""
                        , "▼ Query.findAll [ attribute \"data-test\" \"my-child\" ]"
                        , ""
                        , "    1)  <div class=\"class-on-my-child-0\" data-test=\"my-child\">"
                        , "            <div class=\"class-within-my-child-0\">"
                        , "            </div>"
                        , "        </div>"
                        , ""
                        , "    2)  <div data-test=\"my-child\">"
                        , "        </div>"
                        , ""
                        , ""
                        , "▼ Query.index 1"
                        , ""
                        , "    1)  <div data-test=\"my-child\">"
                        , "        </div>"
                        , ""
                        , ""
                        , "▼ Query.find [ class \"class-within-my-child-0\" ]"
                        , ""
                        , "0 matches found for this query."
                        , ""
                        , ""
                        , "✗ Query.find always expects to find 1 element, but it found 0 instead."
                        ]
         , test "Fail" <|
            \_ ->
                J.startView subjectView
                    |> J.fail "Explicit failure made"
                    |> J.finish
                    |> Test.Runner.getFailureReason
                    |> expectFailureDescription
                        [ "✗ fail:"
                        , "Explicit failure made"
                        ]
         ]
        )
