module Ginger.Error exposing
    ( ErrorMessage
    , errormessage
    )

{-| Use this package to swap out errors for nice, readable messages.


# Error messages

@docs ErrorMessage
@docs errormessage

-}

import Ginger.Translation as Translation exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http


{-| -}
type alias ErrorMessage =
    { title : String
    , body : String
    }


{-| `errormessage` takes a [Ginger.Translation.Language](https://package.elm-lang.org/packages/driebit/elm-ginger/latest/Ginger-Translation#Language)
(EN or NL) and an [Http.Error](https://package.elm-lang.org/packages/elm/http/latest/Http#Error):

         errormessage EN (Http.BadStatus 404)

This function will give you the following `ErrorMessage`:

         { title = "404 Not Found"
         , body = "Sorry, the page you are looking for doesn't exist."
         }

-}
errormessage : Language -> Http.Error -> ErrorMessage
errormessage language error =
    let
        ( title_, body_ ) =
            Tuple.mapBoth Translation.fromList Translation.fromList <|
                case error of
                    Http.BadBody string ->
                        ( [ ( EN, "There was a problem reading the data" )
                          , ( NL, "Er was een probleem met het lezen van de data" )
                          ]
                        , [ ( EN, string )
                          , ( NL, string )
                          ]
                        )

                    Http.BadStatus status ->
                        badStatusMessage status

                    Http.BadUrl stringString ->
                        ( [ ( EN, "Bad URL" )
                          , ( NL, "Foute URL" )
                          ]
                        , [ ( EN, "The URL provided is invalid." )
                          , ( NL, "De URL is niet geldig." )
                          ]
                        )

                    Http.NetworkError ->
                        ( [ ( EN, "Network Error" )
                          , ( NL, "Netwerkfout" )
                          ]
                        , [ ( EN, "You don't have an internet connection." )
                          , ( NL, "Er is geen verbinding met het internet." )
                          ]
                        )

                    Http.Timeout ->
                        ( [ ( EN, "Connection Timeout" )
                          , ( NL, "Netwerk Timeout" )
                          ]
                        , [ ( EN, "It took too long to get a response from the server." )
                          , ( NL, "Het duurde te lang om een respons te krijgen van de server." )
                          ]
                        )
    in
    { title = Translation.toString language title_
    , body = Translation.toString language body_
    }


badStatusMessage : Int -> ( List ( Language, String ), List ( Language, String ) )
badStatusMessage status =
    case status of
        403 ->
            ( [ ( EN, "403 No Access" )
              , ( NL, "403 Geen toegang" )
              ]
            , [ ( EN, "Sorry, you don't have access to this page." )
              , ( NL, "Sorry, u heeft geen toegang tot deze pagina." )
              ]
            )

        404 ->
            ( [ ( EN, "404 Not Found" )
              , ( NL, "404 Niet gevonden" )
              ]
            , [ ( EN, "Sorry, the page you are looking for doesn't exist." )
              , ( NL, "De pagina die u zoekt bestaat niet." )
              ]
            )

        410 ->
            ( [ ( EN, "410 Gone" )
              , ( NL, "410 Weg" )
              ]
            , [ ( EN, "Sorry, this page is not available anymore." )
              , ( NL, "Deze pagina bestaat niet meer of is verplaatst." )
              ]
            )

        500 ->
            ( [ ( EN, "500 Internal server error" )
              , ( NL, "500 Serverfout" )
              ]
            , [ ( EN, "Sorry, an error has occurred. Please try again later." )
              , ( NL, "Er is een fout opgetreden met de server. Probeer het later nog eens." )
              ]
            )

        x ->
            ( [ ( EN, String.fromInt x ++ " Error" )
              , ( NL, String.fromInt x ++ " Fout" )
              ]
            , [ ( EN, "Sorry, an error has occurred. Please try again later." )
              , ( NL, "Er is een fout opgetreden. Probeer het later nog eens." )
              ]
            )
