module Ginger.Social exposing
    ( Platform(..)
    , viewShareLinks
    , Options
    , defaultOptions
    , viewShareLinksWith
    )

{-| Share your page on social platforms with these links.
Don't forget to add all the right `meta` tags in the head of your html so
the social platforms can scrape your page in their favourite way. Find out
which ones you might need here: <https://megatags.co/reference/>


# Social Platforms

@docs Platform


# Basic sharing

Just a list of links, and nothing else.

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
import Url exposing (Url)


{-| These are the social platforms that are supported in this package. Pass a
list of the ones you want to use to one of the view functions below to get a
list of share links on your page.
-}
type Platform
    = Facebook
    | LinkedIn
    | Twitter
    | Weibo


{-| Your customized `a11yString` will be inside the visually hidden span,
before the platform name, i.e. _Share this page on_ (Twitter/Facebook/etc)
-}
type alias Options =
    { a11yString : String }


{-|

        defaultOptions =
            { a11yString = "Share this page on" }

-}
defaultOptions : Options
defaultOptions =
    { a11yString = "Share this page on" }


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
                    , rel "noopener noreferrer"
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
    toHtml defaultOptions (List.map (\p -> ( p, text "" )) platformList) pageUrl pageTitle


{-| `viewShareLinksWith` takes `defaultOptions`, a list of tuples of social
platforms and the corresponding extra html content for the sharelink, and the
full url and the title of the page you want to share.

        view =
            viewShareLinksWith
                { defaultOptions
                    | a11yString = "Deel deze pagina op"
                }
                [ ( Facebook, img [] [] )
                , ( LinkedIn, img [] [] )
                , ( Twitter, img [] [] )
                , ( Weibo, img [] [] )
                ]
                yourPageUrl
                yourTitle

This function will render an `ul` with a `li` for every Platform you specify,
which will look like this:

        ul []
            [ li []
                [ a
                    [ href SHARE-URL
                    , target "_blank"
                    , rel "noopener noreferrer"
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
viewShareLinksWith : Options -> List ( Platform, Html msg ) -> Url -> String -> Html msg
viewShareLinksWith options platformAndHtmlList pageUrl pageTitle =
    toHtml options platformAndHtmlList pageUrl pageTitle


toHtml : Options -> List ( Platform, Html msg ) -> Url -> String -> Html msg
toHtml options platformAndHtmlList pageUrl pageTitle =
    ul [] <|
        List.map (viewShareLink options << buildLink ( Url.toString pageUrl, pageTitle ))
            platformAndHtmlList


viewShareLink : Options -> ShareLink msg -> Html msg
viewShareLink { a11yString } { platformName, shareUrl, content } =
    li []
        [ a
            [ href shareUrl
            , target "_blank"
            , rel "noopener noreferrer"
            , title platformName
            ]
            [ span
                [ style "position" "absolute"
                , style "clip" "rect(1px,1px,1px,1px)"
                , style "overflow" "hidden"
                , style "height" "1px"
                , style "width" "1px"
                , style "word-wrap" "normal"
                , style "white-space" "nowrap"
                ]
                [ text <| String.join "" [ a11yString, " ", platformName ] ]
            , content
            ]
        ]


type alias ShareLink msg =
    { platformName : String
    , shareUrl : String
    , content : Html msg
    }


buildLink : ( String, String ) -> ( Platform, Html msg ) -> ShareLink msg
buildLink ( url, title ) ( platform, extraHtml ) =
    case platform of
        Facebook ->
            { platformName = "Facebook"
            , shareUrl = String.join "" [ "http://www.facebook.com/sharer.php?u=", url, "&t=", title ]
            , content = extraHtml
            }

        LinkedIn ->
            { platformName = "LinkedIn"
            , shareUrl = String.join "" [ "https://www.linkedin.com/shareArticle?mini=true&url=", url, "&title=", title ]
            , content = extraHtml
            }

        Twitter ->
            { platformName = "Twitter"
            , shareUrl = String.join "" [ "http://twitter.com/share?text=", title, "&url=", url ]
            , content = extraHtml
            }

        Weibo ->
            { platformName = "Weibo"
            , shareUrl = String.join "" [ "http://service.weibo.com/share/share.php?url=", url, "&title=", title ]
            , content = extraHtml
            }
