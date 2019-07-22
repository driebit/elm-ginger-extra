module ViewTest exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Ginger.Error
import Ginger.Social exposing (..)
import Ginger.Translation as Translation exposing (Language)
import Ginger.View
import Html exposing (a, img, li, span, ul)
import Html.Attributes exposing (..)
import Http
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (..)
import Url


suite : Test
suite =
    describe "Elm Ginger Extra"
        [ describe "Ginger.View"
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
            ]
        , describe "Ginger.Social"
            [ test "viewShareLinks renders a list of share links" <|
                \() ->
                    let
                        url =
                            { protocol = Url.Https
                            , host = "example.com"
                            , port_ = Nothing
                            , path = "/page"
                            , query = Nothing
                            , fragment = Nothing
                            }
                    in
                    Ginger.Social.viewShareLinks [ Ginger.Social.Twitter, Ginger.Social.Facebook ]
                        url
                        "Title"
                        |> Query.fromHtml
                        |> Query.findAll [ tag "li" ]
                        |> Query.first
                        |> Query.contains
                            [ li []
                                [ a
                                    [ href "http://twitter.com/share?text=Title&url=https://example.com/page"
                                    , target "_blank"
                                    , rel "noopener noreferrer"
                                    , title "Twitter"
                                    ]
                                    [ span
                                        [ Html.Attributes.style "position" "absolute"
                                        , Html.Attributes.style "clip" "rect(1px,1px,1px,1px)"
                                        , Html.Attributes.style "overflow" "hidden"
                                        , Html.Attributes.style "height" "1px"
                                        , Html.Attributes.style "width" "1px"
                                        , Html.Attributes.style "word-wrap" "normal"
                                        , Html.Attributes.style "white-space" "nowrap"
                                        ]
                                        [ Html.text "Share this page on Twitter" ]
                                    , Html.text ""
                                    ]
                                ]
                            ]
            , test "viewShareLinksWith renders a list of share links" <|
                \() ->
                    let
                        url =
                            { protocol = Url.Https
                            , host = "example.com"
                            , port_ = Nothing
                            , path = "/page"
                            , query = Nothing
                            , fragment = Nothing
                            }
                    in
                    Ginger.Social.viewShareLinksWith
                        { defaultOptions
                            | a11yString = "Deel deze pagina op"
                        }
                        [ ( Ginger.Social.Twitter, img [] [] ), ( Ginger.Social.Facebook, img [] [] ) ]
                        url
                        "Title"
                        |> Query.fromHtml
                        |> Query.findAll [ tag "li" ]
                        |> Query.index 1
                        |> Query.contains
                            [ li []
                                [ a
                                    [ href "http://www.facebook.com/sharer.php?u=https://example.com/page&t=Title"
                                    , target "_blank"
                                    , rel "noopener noreferrer"
                                    , title "Facebook"
                                    ]
                                    [ span
                                        [ Html.Attributes.style "position" "absolute"
                                        , Html.Attributes.style "clip" "rect(1px,1px,1px,1px)"
                                        , Html.Attributes.style "overflow" "hidden"
                                        , Html.Attributes.style "height" "1px"
                                        , Html.Attributes.style "width" "1px"
                                        , Html.Attributes.style "word-wrap" "normal"
                                        , Html.Attributes.style "white-space" "nowrap"
                                        ]
                                        [ Html.text "Deel deze pagina op Facebook" ]
                                    , img [] []
                                    ]
                                ]
                            ]
            ]
        , describe "Ginger.Error"
            [ test "Gives an English 404 message" <|
                \() ->
                    Ginger.Error.errormessage EN (Http.BadStatus 404)
                        |> Expect.equal { title = "404 Not Found", body = "Sorry, the page you are looking for doesn't exist." }
            ]
        ]
