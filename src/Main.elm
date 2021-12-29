module Main exposing (..)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, string)
import Http exposing (Error(..))


-- MAIN

main : Program () Model Msg
main =
  Browser.element
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view 
    }


--MODEL

type Model
  = Failure Http.Error
  | Loading
  | Success String

init : () -> (Model, Cmd Msg) 
init _ =
  (Loading, getRandomCatGif)


-- UPDATE

type Msg
  = MorePlease
  | GotGif (Result Http.Error String)


update : Msg -> Model -> (Model, Cmd Msg)
update msg _ =
  case msg of
    MorePlease ->
      (Loading, getRandomCatGif)

    GotGif result ->
      case result of
        Ok url ->
          (Success url, Cmd.none)

        Err error ->
          (Failure error, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
      Failure error->
        div []
          [ text "I could not load a random cat for some reason."
          , button [ onClick MorePlease] [text "Try Again!"]
          , div[][]
          , text  (errorToString error)
          ]

      Loading ->
        text "Loading..."


      Success url ->
        div 
          [ style "position" "absolute"
          , style "top" "25%"
          , style "left" "25%" ]
          [ button 
            [ onClick MorePlease
            , style "padding" "50px 50px"
            , style "font-size" "20px"
            , style "background-color" "#4CAF50"
            , style "border" "none"
            , style "display" "inline-block"
            ]
            [ text "Random Pic Button"]
            , div [ style "font-size" "30px",style "padding-top" "50px", style "padding-bottom" "50px"][ text "LOVE IMAGE"]
            , img [src url] []
          ]



-- HTTP

getRandomCatGif : Cmd Msg
getRandomCatGif =
  Http.get
    { url = "https://api.giphy.com/v1/gifs/random?api_key=U4quJu1HFIM8YtO62U8M6t21dtAx0A83&tag=love&rating=g"
    , expect = Http.expectJson GotGif gifDecoder
    }



gifDecoder : Decoder String
gifDecoder =
  field "data" (field "images" (field "original" (field "url" string))) 

errorToString : Http.Error -> String
errorToString error =
    case error of
        BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Timeout ->
            "Unable to reach the server, try again"
        NetworkError ->
            "Unable to reach the server, check your network connection"
        BadStatus 500 ->
            "The server had a problem, try again later"
        BadStatus 400 ->
            "Verify your information and try again"
        BadStatus _ ->
            "Unknown error"
        BadBody _ ->
            "Bad Body"