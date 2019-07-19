module Ginger.Social exposing
    ( Platform(..)
    , viewShareLinks
    , Options
    , defaultOptions
    , viewShareLinksWith
    )

{-| Share your page on social platforms with these links.
Don't forget to add all the right metatags in the head of your html so
the platforms can scrape your page in their favourite way. Find out which ones
you might need here: <https://megatags.co/reference/>


# Basic sharing

Just a list of links, and nothing else.

@docs Platform
@docs viewShareLinks


# Sharing with extra options

For when you want to add some extra images or text to your links,
or want to adjust the text for screen reader users.

@docs Options
@docs defaultOptions
@docs viewShareLinksWith

-}

import Ginger.View
import Html exposing (..)
import Html.Attributes exposing (..)
import String.Interpolate exposing (interpolate)
import Url exposing (Url)


{-| First, determine for which social platforms you want to have share links.
These are the ones you can choose from.
-}
type Platform
    = Facebook
    | LinkedIn
    | Twitter
    | Weibo


{-| Your `extraHtml` will be placed inside the `a` of the share link, and your
customized `a11yString` will be inside the visually hidden span, before the
platform name.
-}
type alias Options msg =
    { extraHtml : Html msg
    , a11yString : String
    }


{-|

        defaultOptions =
            { extraHtml = text ""
            , a11yString = "Share this page on"
            }

-}
defaultOptions : Options msg
defaultOptions =
    { extraHtml = text ""
    , a11yString = "Share this page on"
    }


{-| `viewShareLinks` takes a list of platforms, the full url of the page you
want to share, and the title of that page.

        view =
            viewShareLinks [ Twitter, Facebook ]
                yourPageUrl "Your Page Title"

This function will render an `ul` with a `li` for every Platform you specify:

        ul []
            [ li []
                [ a
                    [ href SHARE-URL
                    , target "_blank"
                    , title PLATFORMNAME
                    ]
                    [ span [ VISUALLY-HIDDEN-STYLES ]
                        [ Share this page on PLATFORMNAME ]
                    ]
                ]
            , li [] [...]
            , li [] [...]
            ]

With the url and the title, the `SHARE-URL` and `PLATFORMNAME` are built for
each social platform. The `span` inside the `a` makes the link accessible to
screen readers and is invisible to sighted users, in keeping with accessibility
(a11y) standards.
These are the `VISUALLY-HIDDEN-STYLES`:

        [ style "position" "absolute"
        , style "clip" "rect(1px,1px,1px,1px)"
        , style "overflow" "hidden"
        , style "height" "1px"
        , style "width" "1px"
        , style "word-wrap" "normal"
        , style "white-space" "nowrap"
        ]

-}
viewShareLinks : List Platform -> Url -> String -> Html msg
viewShareLinks platformList pageUrl pageTitle =
    toHtml defaultOptions platformList pageUrl pageTitle


{-|

        view =
            viewShareLinksWith
                { defaultOptions
                    | extraHtml = img [] []
                    , a11yString = "Deel deze pagina op"
                }
                [ Facebook, LinkedIn, Twitter, Weibo ]
                yourPageUrl
                yourTitle

This function will render an `ul` with a `li` for every Platform you specify:

        ul []
            [ li []
                [ a
                    [ href SHARE-URL
                    , target "_blank"
                    , title PLATFORMNAME
                    ]
                    [ span [ VISUALLY-HIDDEN-STYLES ]
                        [ Deel deze pagina op PLATFORMNAME ]
                    , img [] []
                    ]
                ]
            , li [] [...]
            , li [] [...]
            ]

-}
viewShareLinksWith : Options msg -> List Platform -> Url -> String -> Html msg
viewShareLinksWith options platformList pageUrl pageTitle =
    toHtml options platformList pageUrl pageTitle


vieww : Url -> String -> Html msg
vieww url title =
    viewShareLinks [ Facebook, LinkedIn, Twitter, Weibo ]
        url
        title


toHtml : Options msg -> List Platform -> Url -> String -> Html msg
toHtml options platformList pageUrl pageTitle =
    ul [] <|
        List.map (viewShareLink options << buildLink [ Url.toString pageUrl, pageTitle ])
            platformList


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


buildLink : List String -> Platform -> ( String, String )
buildLink urlAndTitle platform =
    Tuple.mapSecond ((|>) urlAndTitle) <|
        case platform of
            Facebook ->
                ( "Facebook"
                , interpolate "http://www.facebook.com/sharer.php?u={0}&t={1}"
                )

            LinkedIn ->
                ( "LinkedIn"
                , interpolate "https://www.linkedin.com/shareArticle?mini=true&url={0}&title={1}"
                )

            Twitter ->
                ( "Twitter"
                , interpolate "http://twitter.com/share?text={1}&url={0}"
                )

            Weibo ->
                ( "Weibo"
                , interpolate "http://service.weibo.com/share/share.php?url={0}&title={1}"
                )
