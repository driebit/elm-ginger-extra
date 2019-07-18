module Ginger.View exposing
    ( viewIf
    , viewIfNot
    , viewEither
    , viewMaybe
    , viewMaybeWithDefault
    )

{-| This library contains some useful view functions for when
the rendering of your html depends on booleans and maybes.


# Conditional views

Here is an example model of a resource:

    type alias Model =
        { title : String
        , category : Ginger.Category
        , author : Maybe String
        , body : Html msg
        }

@docs viewIf
@docs viewIfNot
@docs viewEither


# Optional views

An example model of a resource:

    type alias Model =
        { title : String
        , category : Ginger.Category
        , author : Maybe String
        , body : Html msg
        }

@docs viewMaybe
@docs viewMaybeWithDefault

-}

import Html exposing (..)


{-| Render some html if a boolean expression evaluates to True.

        view : Model -> Html msg
        view model =
            article []
                [ h1 [] [ text model.title ]
                , viewIf
                    (model.category == Category.Article)
                    (\ -> p [] [ text "article"])
                ]

-}
viewIf : Bool -> (() -> Html msg) -> Html msg
viewIf bool html1 =
    if bool then
        html1 ()

    else
        text ""


{-| Render some html if a boolean expression evaluates to False.
-}
viewIfNot : Bool -> (() -> Html msg) -> Html msg
viewIfNot bool html1 =
    viewIf (not bool) html1


{-| If the boolean expression evaluates to True, render some html, otherwise,
render some other html.

        view : Model -> Html msg
        view model =
            , article []
                [ h1 [] [ text model.title ]
                , p []
                    [ viewEither (model.category == Category.Article)
                        (\_ -> text "article")
                        (\_ -> text "something else")
                    ]
                ]

-}
viewEither : Bool -> (() -> Html msg) -> (() -> Html msg) -> Html msg
viewEither bool html1 html2 =
    if bool then
        html1 ()

    else
        html2 ()


{-| Maybe, the resource has an author. Render what's in the `maybe`, or nothing.

    view : Model -> Html msg
    view model =
        article []
            [ h1 [] [ text "Article" ]
            , viewMaybe model.author
                (\authorName -> p [] [ text authorName ])
            ]

-}
viewMaybe : Maybe a -> (a -> Html msg) -> Html msg
viewMaybe maybeA html1 =
    case maybeA of
        Just a ->
            html1 a

        Nothing ->
            text ""


{-| Render something else if there's nothing in the `maybe`.

    view : Model -> Html msg
    view model =
        article []
            [ h1 [] [ text "Article" ]
            , p []
                [ viewMaybeWithDefault model.author
                    (\authorName -> text authorName)
                    (text "Anonymous")
                ]
            ]

-}
viewMaybeWithDefault : Maybe a -> (a -> Html msg) -> Html msg -> Html msg
viewMaybeWithDefault maybeA html1 html2 =
    case maybeA of
        Just a ->
            html1 a

        Nothing ->
            html2
