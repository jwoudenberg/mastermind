module Main exposing (main)

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Html.App as App
import Random
import Array


type alias Model =
    { solution : Solution
    , guesses : List Code
    , currentGuess : PartialCode
    }


type Solution
    = Unpicked
    | Picked Code


type alias Code =
    ( Color, Color, Color, Color )


type alias PartialCode =
    ( Maybe Color, Maybe Color, Maybe Color, Maybe Color )


type Color
    = Red
    | Yellow
    | Blue
    | Green
    | Purple
    | White


type Place
    = First
    | Second
    | Third
    | Fourth


type GameState
    = Won
    | Lost
    | Loading
    | Playing


type Msg
    = PickCode Code
    | SelectColor Place Color


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        PickCode code ->
            ( { model | solution = Picked code }
            , Cmd.none
            )

        SelectColor place color ->
            ({ model | currentGuess = modifyGuess place color model.currentGuess })


modifyGuess : Place -> Color -> PartialCode -> PartialCode
modifyGuess place color ( first, second, third, fourth ) =
    case place of
        First ->
            ( color, second, third, fourth )

        Second ->
            ( first, color, third, fourth )

        Third ->
            ( first, second, color, fourth )

        Fourth ->
            ( first, second, third, color )


colors : List Color
colors =
    [ Red, Yellow, Blue, Green, Purple, White ]


pickSolution : Cmd Msg
pickSolution =
    Random.generate PickCode generator


pickRandom : List a -> Random.Generator (Maybe a)
pickRandom list =
    Random.int 0 (-1 + List.length colors)
        |> Random.map (\index -> Array.get index (Array.fromList list))


pickRandomColor : Random.Generator Color
pickRandomColor =
    Random.map (Maybe.withDefault Red) (pickRandom colors)


generator : Random.Generator Code
generator =
    Random.map4 (,,,) pickRandomColor pickRandomColor pickRandomColor pickRandomColor


emptyGuess : PartialCode
emptyGuess =
    ( Nothing, Nothing, Nothing, Nothing )


main : Program Never
main =
    App.program
        { init =
            ( { solution = Unpicked, guesses = [], currentGuess = emptyGuess }
            , pickSolution
            )
        , subscriptions = \_ -> Sub.none
        , update = update
        , view = view
        }


view : Model -> Html.Html Msg
view { solution } =
    Html.div
        []
        [ guessView
        , solutionView solution
        ]


guessView : Html.Html Msg
guessView =
    Html.div
        []
        (List.repeat 4 colorSelect)


colorSelect : Html.Html Msg
colorSelect =
    Html.select
        [ Html.Events.on "change" (Json.Decode.at [ "target", "value" ]) SelectColor
        ]
        (List.map colorOption colors)


colorOption : Color -> Html.Html Msg
colorOption color =
    Html.option
        [ Attributes.value (toString color) ]
        [ (Html.text <| toString color) ]


solutionView : Solution -> Html.Html Msg
solutionView solution =
    case solution of
        Unpicked ->
            Html.text "Unpicked"

        Picked code ->
            Html.text <| toString code
