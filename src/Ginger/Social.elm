module Ginger.Social exposing
    ( Platform(..)
    , Ingredients
    , viewShareLinks1
    )

{-| Share your page on social platforms with these links.
Don't forget to add all the right metatags in the head of your html so
the platforms can scrape your page in their favourite way. Find out which ones
you might need here: <https://megatags.co/reference/>


# Social sharing

@docs Platform
@docs Ingredients
@docs viewShareLinks

-}

import Ginger.View
import Html exposing (..)
import Html.Attributes exposing (..)
import String.Interpolate exposing (interpolate)


{-| First, determine for which social platforms you want to have share links.
These are the ones you can choose from.
-}
type Platform
    = Facebook
    | LinkedIn
    | Twitter
    | Weibo


{-| To build a social share link, you need proper ingredients. The `pageUrl` is
the full url of the page: "<https://www.yourdomain.com/yourpage>". The `pageTitle` is
the title of your page: "Your Page". And last but not least, you can add some
`extraHtml` to display inside of the link, like an svg icon or some text.
-}
type alias Ingredients =
    { pageUrl : String
    , pageTitle : String
    }


{-|

        view =
            viewShareLinks [ Twitter, Facebook ] <|
                Ingredients "url" "title" Nothing

`viewShareLinks` will render an `ul` with `li` for every Platform:

        ul []
            [ li []
                [ a
                    [ href SHARE-URL
                    , target "_blank"
                    , title PLATFORMNAME
                    ]
                    [ span [ VISUALLY-HIDDEN-STYLES ]
                        [ Share this page on PLATFORMNAME ]
                    , Ingredients.extraHtml
                    ]
                ]
            , li [] [...]
            , li [] [...]
            ]

With `Ingredients.pageUrl` and `Ingredients.pageTitle`, the `SHARE-URL` and
`PLATFORMNAME` are built for each social platform. The `span` inside the `a`
makes the link accessible to screen readers. These are the `VISUALLY-HIDDEN-STYLES`:

        [ style "position" "absolute"
        , style "clip" "rect(1px,1px,1px,1px)"
        , style "overflow" "hidden"
        , style "height" "1px"
        , style "width" "1px"
        , style "word-wrap" "normal"
        , style "white-space" "nowrap"
        ]

-}
type alias Options msg =
    { extraHtml : Html msg
    , a11yString : String
    }


defaultOptions : Options msg
defaultOptions =
    { extraHtml = text ""
    , a11yString = "Share this page on"
    }


type ShareLinks msg
    = ShareLinks (Options msg) (List Platform) Ingredients


viewShareLinks1 : List Platform -> Ingredients -> ShareLinks msg
viewShareLinks1 platformList ingredients =
    ShareLinks defaultOptions platformList ingredients


test =
    div []
        [ viewShareLinks1 [] { pageUrl = "foo", pageTitle = "bar" }
            |> withExtraHtml (text "baz")
            |> withA11yString "hola"
            |> toHtml
        ]


withExtraHtml : Html msg -> ShareLinks msg -> ShareLinks msg
withExtraHtml html (ShareLinks options platformList ingredients) =
    ShareLinks { options | extraHtml = html } platformList ingredients


withA11yString : String -> ShareLinks msg -> ShareLinks msg
withA11yString str (ShareLinks options platformList ingredients) =
    ShareLinks { options | a11yString = str } platformList ingredients



--


toHtml : ShareLinks msg -> Html msg
toHtml (ShareLinks options platformList { pageUrl, pageTitle }) =
    ul [] <|
        List.map (viewShareLink options) <|
            buildShareLinks
                platformList
                [ pageUrl, pageTitle ]


viewShareLink : Options msg -> ( String, String ) -> Html msg
viewShareLink { a11yString, extraHtml } ( platformName, shareUrl ) =
    li []
        [ a
            [ href shareUrl
            , target "_blank"
            , title platformName
            ]
            [ extraHtml
            , span
                [ style "position" "absolute"
                , style "clip" "rect(1px,1px,1px,1px)"
                , style "overflow" "hidden"
                , style "height" "1px"
                , style "width" "1px"
                , style "word-wrap" "normal"
                , style "white-space" "nowrap"
                ]
                [ text <| interpolate "{0} {1}" [ a11yString, platformName ] ]
            ]
        ]


buildShareLinks : List Platform -> List String -> List ( String, String )
buildShareLinks platformList urlAndTitle =
    List.map (buildLink urlAndTitle) platformList


buildLink : List String -> Platform -> ( String, String )
buildLink urlAndTitle platform =
    let
        build =
            Tuple.mapSecond ((|>) urlAndTitle)
    in
    case platform of
        Facebook ->
            build
                ( "Facebook"
                , interpolate "http://www.facebook.com/sharer.php?u={0}&t={1}"
                )

        LinkedIn ->
            build
                ( "LinkedIn"
                , interpolate "https://www.linkedin.com/shareArticle?mini=true&url={0}&title={1}"
                )

        Twitter ->
            build
                ( "Twitter"
                , interpolate "http://twitter.com/share?text={1}&url={0}"
                )

        Weibo ->
            build
                ( "Weibo"
                , interpolate "http://service.weibo.com/share/share.php?url={0}&title={1}"
                )
