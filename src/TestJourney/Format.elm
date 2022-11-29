module TestJourney.Format exposing (format)

import Test.Runner.Failure

verticalBar : String -> String -> String -> String
verticalBar comparison expected actual =
    [ actual
    , "╵"
    , "│ " ++ comparison
    , "╷"
    , expected
    ]
        |> String.join "\n"


toStringLists : List String -> String
toStringLists =
    String.join ", "


listDiffToString :
    Int
    -> String
    -> { expected : List String, actual : List String }
    -> { originalExpected : List String, originalActual : List String }
    -> String
listDiffToString index description { expected, actual } originals =
    case ( expected, actual ) of
        ( [], [] ) ->
            [ "Two lists were unequal previously, yet ended up equal later."
            , "This should never happen!"
            , "Please report this bug to https://github.com/elm-community/elm-test/issues - and include these lists: "
            , "\n"
            , toStringLists originals.originalExpected
            , "\n"
            , toStringLists originals.originalActual
            ]
                |> String.join ""

        ( _ :: _, [] ) ->
            verticalBar (description ++ " was shorter than")
                (toStringLists originals.originalExpected)
                (toStringLists originals.originalActual)

        ( [], _ :: _ ) ->
            verticalBar (description ++ " was longer than")
                (toStringLists originals.originalExpected)
                (toStringLists originals.originalActual)

        ( firstExpected :: restExpected, firstActual :: restActual ) ->
            if firstExpected == firstActual then
                -- They're still the same so far; keep going.
                listDiffToString (index + 1)
                    description
                    { expected = restExpected
                    , actual = restActual
                    }
                    originals

            else
                -- We found elements that differ; fail!
                String.join ""
                    [ verticalBar description
                        (toStringLists originals.originalExpected)
                        (toStringLists originals.originalActual)
                    , "\n\nThe first diff is at index "
                    , String.fromInt index
                    , ": it was `"
                    , firstActual
                    , "`, but `"
                    , firstExpected
                    , "` was expected."
                    ]

format : String -> Test.Runner.Failure.Reason -> String
format description reason =
    case reason of
        Test.Runner.Failure.Custom ->
            description

        Test.Runner.Failure.Equality e a ->
            verticalBar description e a

        Test.Runner.Failure.Comparison e a ->
            verticalBar description e a

        Test.Runner.Failure.TODO ->
            description

        Test.Runner.Failure.Invalid Test.Runner.Failure.BadDescription ->
            if description == "" then
                "The empty string is not a valid test description."

            else
                "This is an invalid test description: " ++ description

        Test.Runner.Failure.Invalid _ ->
            description

        Test.Runner.Failure.ListDiff expected actual ->
            listDiffToString 0
                description
                { expected = expected
                , actual = actual
                }
                { originalExpected = expected
                , originalActual = actual
                }

        Test.Runner.Failure.CollectionDiff { expected, actual, extra, missing } ->
            let
                extraStr =
                    if List.isEmpty extra then
                        ""

                    else
                        "\nThese keys are extra: "
                            ++ (extra |> String.join ", " |> (\d -> "[ " ++ d ++ " ]"))

                missingStr =
                    if List.isEmpty missing then
                        ""

                    else
                        "\nThese keys are missing: "
                            ++ (missing |> String.join ", " |> (\d -> "[ " ++ d ++ " ]"))
            in
            String.join ""
                [ verticalBar description expected actual
                , "\n"
                , extraStr
                , missingStr
                ]