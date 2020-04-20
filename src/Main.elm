module Main exposing (main)

import Browser
import Dict
import Dict.Extra as Dict
import File exposing (File)
import File.Select as Select
import Html exposing (Html, div, button, text, h1)
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
    Html Msg


init : () -> ( Model, Cmd Msg )
init _ =
    ( text "", Cmd.none )



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
                |> String.lines
                |> List.drop 1 --Drop header row
                |> List.map toVoteRow
                |> sortResults
                |> List.map (text >> List.singleton >> Html.li [])
                |> Html.ol []
            , Cmd.none
            )

toVoteRow : String -> List String
-- Strip metadata from vote rows and split into choices ordered by preference.
toVoteRow =
    String.split "," >> List.drop 5

rowsToSortedTallies : List (List String) -> List (String, Int)
-- Tallies first-choice votes as ("Choice", # votes received), sorted high to low
rowsToSortedTallies rows =
    let firstChoices = rows |> List.filterMap List.head |> Debug.log "First choices: "
    in Dict.frequencies firstChoices 
        |> Dict.toList
        |> List.sortBy Tuple.second
        |> List.reverse
        |> Debug.log "Sorted Tallies: "

sortResults : List (List String) -> List String
sortResults = nextRound [] []

nextRound : List String -> List String -> List (List String) -> List String
nextRound winners losers voteRows =
    let
        tallies = voteRows |> Debug.log ("*New Round!*\nVote rows") |> rowsToSortedTallies

        (firstPlaceName, firstPlaceCount) = tallies |> List.head |> Maybe.withDefault ("", 0)

        totalVotes = tallies |> List.map Tuple.second |> List.sum

        isMajority = firstPlaceCount * 2 > totalVotes

        lastPlaceName = List.last tallies |> Maybe.map Tuple.first |> Maybe.withDefault ""

        removeName name = List.map (List.filter ((/=) name)) voteRows
    in
    if List.isEmpty tallies
        then winners ++ losers
        else if isMajority
            then nextRound (winners ++ [firstPlaceName]) losers (removeName firstPlaceName)
            else nextRound winners (lastPlaceName :: losers)  (removeName lastPlaceName)



-- VIEW


view : Model -> Html Msg
view model =
    div [style "white-space" "pre"]
        [ h1 [] [text "Ranked Choice Vote Counter"]
        , text "This code assumes there's a header row and five extranneous columns at the beginning of every vote submission. I could make this more customizable if needed.\nA sanity check on the results will print to the web console."
        , button [ onClick CsvRequested ] [ text "Load CSV" ]
        , model
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
