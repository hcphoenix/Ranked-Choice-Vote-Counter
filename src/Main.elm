module Main exposing (..)

import Browser
import Dict
import Dict.Extra as Dict
import File exposing (File)
import File.Select as Select
import Html exposing (Html, div, button, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import List.Extra as List
import Task



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    String


init : () -> ( Model, Cmd Msg )
init _ =
    ( "", Cmd.none )



-- UPDATE


type Msg
    = CsvRequested
    | CsvSelected File
    | CsvLoaded String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvRequested ->
            ( model
            , Select.file [ "text/csv" ] CsvSelected
            )

        CsvSelected file ->
            ( model
            , Task.perform CsvLoaded (File.toString file)
            )

        CsvLoaded content ->
            ( content
                |> makeLineList
                |> List.map toResultRow
                |> Debug.log "Result rows: "
                |> getWinner
                |> (++) "Winner: "
            , Cmd.none
            )



-- Strip header row and get a list of entries


makeLineList : String -> List String
makeLineList =
    String.split "\n" >> List.drop 1



-- Strip metadata from result rows and split into choices ordered by preference


toResultRow : String -> List String
toResultRow =
    String.split "," >> List.drop 5


getWinner : List (List String) -> String
getWinner resultRows =
    let
        firstChoices =
            resultRows |> List.filterMap List.head |> Debug.log "NEW ROUND!\nFirst choices: "

        tallies =
            Dict.frequencies firstChoices |> Dict.toList |> Debug.log "Tallies: "

        (firstPlaceName, firstPlaceCount) =
            tallies |> List.maximumBy Tuple.second |> Maybe.withDefault ( "No first place...? :(", -1 ) |> Debug.log "First place this round: "

        (lastPlaceName, _) =
            tallies |> List.minimumBy Tuple.second |> Maybe.withDefault ( "No last place...? :(", -1 ) |> Debug.log "Last place this round: "

        isMajority =
            firstPlaceCount * 2 > (tallies |> List.map Tuple.second |> List.sum)

        filterLastPlace : List (List String) -> List (List String)
        filterLastPlace =
            List.map (List.filter ((/=) lastPlaceName))
    in
    if isMajority then firstPlaceName else filterLastPlace resultRows |> getWinner



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        "" ->
            div [] [
                p [] [ text "This code assumes there's a header row and five extranneous columns at the beginning of every vote submission. I could make this more customizable if needed."]
                , p [] [ text "A sanity check on the results will print to the web console."]
                , button [ onClick CsvRequested ] [ text "Load CSV" ]
            ]

        _ ->
            p [ style "white-space" "pre" ] [ text model ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
