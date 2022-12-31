module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (..)
import List.Extra



---- MODEL ----


type alias Model =
    { players : List String
    }


init : ( Model, Cmd Msg )
init =
    ( { players = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = AddPlayer
    | EditPlayerName Int String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlayer ->
            ( { model | players = model.players ++ [ "" ] }
            , Cmd.none
            )

        EditPlayerName index name ->
            ( { model
                | players =
                    model.players |> List.Extra.setAt index name
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        [ Attributes.css
            [ minHeight (vh 100)
            , maxWidth (vw 100)
            , property "display" "grid"
            , property "grid-template-columns" "1fr"
            , property "justify-items" "center"
            ]
        ]
        [ div
            [ Attributes.css
                [ maxWidth (rem 20)
                , width (pc 100)
                , padding (rem 2)
                , property "display" "grid"
                , property "grid-gap" "0.75rem"
                , property "grid-template-columns" "1fr"
                , property "grid-template-rows" "repeat(auto-fit, 2rem)"
                ]
            ]
            (List.indexedMap
                editNameList
                model.players
                ++ [ button [ onClick AddPlayer ] [ text "Agregar jugador" ] ]
            )
        ]


editNameList : Int -> String -> Html Msg
editNameList index name =
    input
        [ Attributes.value name
        , onInput (EditPlayerName index)
        ]
        []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
