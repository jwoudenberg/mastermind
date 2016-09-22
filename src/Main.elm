module Main exposing (main)

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Html.App as App
import Json.Decode as Decode
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
    | MakeGuess


asList : ( a, a, a, a ) -> List a
asList ( a, b, c, d ) =
    [ a, b, c, d ]


places : List Place
places =
    [ First, Second, Third, Fourth ]


get : Place -> ( a, a, a, a ) -> a
get place ( a, b, c, d ) =
    case place of
        First ->
            a

        Second ->
            b

        Third ->
            c

        Fourth ->
            d


set : Place -> a -> ( a, a, a, a ) -> ( a, a, a, a )
set place x ( a, b, c, d ) =
    case place of
        First ->
            ( x, b, c, d )

        Second ->
            ( a, x, c, d )

        Third ->
            ( a, b, x, d )

        Fourth ->
            ( a, b, c, x )


full : PartialCode -> Maybe Code
full partialCode =
    case partialCode of
        ( Just a, Just b, Just c, Just d ) ->
            Just ( a, b, c, d )

        _ ->
            Nothing


update : Msg -> Model -> ( Model, Cmd a )
update msg model =
    case msg of
        PickCode code ->
            ( { model | solution = Picked code }
            , Cmd.none
            )

        SelectColor place color ->
            ( { model | currentGuess = set place (Just color) model.currentGuess }
            , Cmd.none
            )

        MakeGuess ->
            case (full model.currentGuess) of
                Nothing ->
                    ( model, Cmd.none )

                Just code ->
                    ( { model | currentGuess = emptyGuess, guesses = model.guesses ++ [ code ] }, Cmd.none )


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
view { solution, currentGuess, guesses } =
    Html.div
        []
        [ guessLogView guesses
        , currentGuesView currentGuess
        , guessButtonView
        , solutionView solution
        ]


guessLogView : List Code -> Html.Html Msg
guessLogView guesses =
    Html.ul
        []
        (List.map guessView guesses)


guessView : Code -> Html.Html Msg
guessView code =
    Html.li
        []
        (List.map colorView (asList code))


colorView : Color -> Html.Html Msg
colorView =
    (toString >> Html.text)


guessButtonView : Html.Html Msg
guessButtonView =
    Html.button
        [ Events.onClick MakeGuess ]
        [ Html.text "Guess!" ]


currentGuesView : PartialCode -> Html.Html Msg
currentGuesView guess =
    Html.div
        []
        (List.map2 colorSelect places (asList guess))


colorSelect : Place -> Maybe Color -> Html.Html Msg
colorSelect place currentColor =
    Html.select
        [ Events.on "change"
            (Decode.at [ "target", "value" ] Decode.string
                |> Decode.map (colorFromString >> (SelectColor place))
            )
        ]
        ((unpickedOption currentColor) :: (List.map (colorOption currentColor) colors))


unpickedOption : Maybe Color -> Html.Html Msg
unpickedOption maybeSelectedColor =
    Html.option
        [ Attributes.value "unpicked"
        , Attributes.selected
            (case maybeSelectedColor of
                Just _ ->
                    False

                Nothing ->
                    True
            )
        ]
        [ Html.text "" ]


colorFromString : String -> Color
colorFromString colorString =
    case colorString of
        "Red" ->
            Red

        "Yellow" ->
            Yellow

        "Blue" ->
            Blue

        "Green" ->
            Green

        "Purple" ->
            Purple

        "White" ->
            White

        -- We need some fallback to make the compiler happy
        _ ->
            Red


colorOption : Maybe Color -> Color -> Html.Html Msg
colorOption maybeSelectedColor ownColor =
    Html.option
        [ Attributes.value (toString ownColor)
        , Attributes.selected
            (case maybeSelectedColor of
                Just selectedColor ->
                    selectedColor == ownColor

                Nothing ->
                    False
            )
        ]
        [ (Html.text <| toString ownColor) ]


solutionView : Solution -> Html.Html Msg
solutionView solution =
    case solution of
        Unpicked ->
            Html.text "Unpicked"

        Picked code ->
            Html.text <| toString code
