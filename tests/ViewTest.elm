module ViewTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Ginger.View
import Html
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)


suite : Test
suite =
    describe "Elm Ginger Extra"
        [ describe "Conditional views"
            [ test "viewIf renders html if boolean is True" <|
                \() ->
                    Ginger.View.viewIf True
                        (\_ -> Html.text "I should be rendered!")
                        |> Query.fromHtml
                        |> Query.has [ text "I should be rendered!" ]
            , test "viewIf renders no html if boolean is False" <|
                \() ->
                    Ginger.View.viewIf False
                        (\_ -> Html.text "I should not be rendered!")
                        |> Query.fromHtml
                        |> Query.hasNot [ text "I should not be rendered!" ]
            , test "viewIfNot renders html if boolean is False" <|
                \() ->
                    Ginger.View.viewIfNot False
                        (\_ -> Html.text "I should be rendered!")
                        |> Query.fromHtml
                        |> Query.has [ text "I should be rendered!" ]
            , test "viewIfNot renders no html if boolean is True" <|
                \() ->
                    Ginger.View.viewIfNot True
                        (\_ -> Html.text "I should not be rendered!")
                        |> Query.fromHtml
                        |> Query.hasNot [ text "I should not be rendered!" ]
            , test "viewEither renders the first html option if boolean is True" <|
                \() ->
                    Ginger.View.viewEither True
                        (\_ -> Html.text "I am the first option!")
                        (\_ -> Html.text "I am the second option!")
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.has [ text "I am the first option!" ]
                            , Query.hasNot [ text "I am the second option!" ]
                            ]
            , test "viewEither renders the second html option if boolean is False" <|
                \() ->
                    Ginger.View.viewEither False
                        (\_ -> Html.text "I am the first option!")
                        (\_ -> Html.text "I am the second option!")
                        |> Query.fromHtml
                        |> Expect.all
                            [ Query.hasNot [ text "I am the first option!" ]
                            , Query.has [ text "I am the second option!" ]
                            ]
            ]
        , describe "Optional views"
            [ test "viewMaybe renders nothing if there's nothing in the Maybe" <|
                \() ->
                    Ginger.View.viewMaybe Nothing
                        (\_ -> Html.text "Nothing should be rendered!")
                        |> Query.fromHtml
                        |> Query.hasNot [ text "Nothing should be rendered!" ]
            , test "viewMaybe renders something if there's something in the Maybe" <|
                \() ->
                    Ginger.View.viewMaybe (Just "Something")
                        (\x -> Html.text (x ++ " should be rendered!"))
                        |> Query.fromHtml
                        |> Query.has [ text "Something should be rendered!" ]
            , test "viewMaybeWithDefault renders something if there's something in the Maybe" <|
                \() ->
                    Ginger.View.viewMaybeWithDefault (Just "Something")
                        (\x -> Html.text (x ++ " should be rendered!"))
                        (Html.text "I am the default text.")
                        |> Query.fromHtml
                        |> Query.has [ text "Something should be rendered!" ]
            , test "viewMaybeWithDefault renders the default html if there's nothing in the Maybe" <|
                \() ->
                    Ginger.View.viewMaybeWithDefault Nothing
                        (\x -> Html.text (x ++ " should not be rendered!"))
                        (Html.text "I am the default text.")
                        |> Query.fromHtml
                        |> Query.has [ text "I am the default text." ]
            ]

        -- Expect.equal is designed to be used in pipeline style, like this.
        , test "reverses a known string" <|
            \_ ->
                "ABCDEFG"
                    |> String.reverse
                    |> Expect.equal "GFEDCBA"

        -- fuzz runs the test 100 times with randomly-generated inputs!
        , fuzz string "restores the original string if you run it again" <|
            \randomlyGeneratedString ->
                randomlyGeneratedString
                    |> String.reverse
                    |> String.reverse
                    |> Expect.equal randomlyGeneratedString
        ]
