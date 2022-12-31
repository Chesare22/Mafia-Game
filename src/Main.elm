module Main exposing (..)

import Browser
import Css exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (..)
import List.Extra
import Material.Icons as Filled
import Material.Icons.Types exposing (Coloring(..))
import Svg.Styled



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
    | DeletePlayer Int
    | EditPlayerName Int String
    | StartChoosingRoles


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        AddPlayer ->
            ( { model | players = model.players ++ [ "" ] }
            , Cmd.none
            )

        DeletePlayer index ->
            ( { model
                | players =
                    model.players |> List.Extra.removeAt index
              }
            , Cmd.none
            )

        EditPlayerName index name ->
            ( { model
                | players =
                    model.players |> List.Extra.setAt index name
              }
            , Cmd.none
            )

        StartChoosingRoles ->
            let
                validPlayers : List String
                validPlayers =
                    model.players
                        |> List.map String.trim
                        |> List.filter (String.isEmpty >> not)

                repeatedPlayers : List String
                repeatedPlayers =
                    getRepeatedElements validPlayers
            in
            ( model
            , Cmd.none
            )


getRepeatedElements : List a -> List a
getRepeatedElements =
    getRepeatedElements_ []


getRepeatedElements_ : List a -> List a -> List a
getRepeatedElements_ repeatedSoFar list =
    case list of
        [] ->
            repeatedSoFar

        head :: tail ->
            if List.member head tail then
                getRepeatedElements_
                    (head :: repeatedSoFar)
                    (tail |> List.filter ((/=) head))

            else
                getRepeatedElements_ repeatedSoFar tail



---- VIEW ----


view : Model -> Html Msg
view model =
    div
        [ Attributes.css
            [ minHeight (vh 100)
            , maxWidth (vw 100)
            , padding (rem 2)
            , property "display" "grid"
            , property "grid-template-columns" "1fr"
            , property "justify-items" "center"
            ]
        ]
        [ div
            [ Attributes.css
                [ maxWidth (rem 20)
                , width (pct 100)
                , property "display" "grid"
                , property "grid-gap" "0.75rem"
                , property "grid-template-columns" "1fr"
                , property "grid-template-rows" "repeat(auto-fit, 2rem)"
                ]
            ]
            (List.indexedMap
                editNameList
                model.players
                ++ [ button [ onClick AddPlayer ] [ text "Agregar jugador" ]
                   , button [ onClick StartChoosingRoles ] [ text "Elegir Roles" ]
                   ]
            )
        ]


editNameList : Int -> String -> Html Msg
editNameList index name =
    div
        [ Attributes.css
            [ property "display" "grid"
            , property "grid-gap" "0.75rem"
            , property "grid-template-columns" "1fr auto"
            ]
        ]
        [ input
            [ Attributes.value name
            , onInput (EditPlayerName index)
            ]
            []
        , button
            [ onClick (DeletePlayer index)
            ]
            [ Svg.Styled.fromUnstyled <| Filled.delete 20 Inherit ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view >> toUnstyled
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
