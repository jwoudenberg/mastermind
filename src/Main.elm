module Main exposing (main)

import Html
import Html.Attributes as Attributes
import Html.Events as Events
import Html.App as App
import Json.Decode as Decode
import Random
import Array
import String


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


type GameState
    = Won
    | Lost
    | Loading
    | Playing


type Msg
    = PickCode Code
    | SelectColor (Place (Maybe Color)) Color
    | MakeGuess


asList : ( a, a, a, a ) -> List a
asList ( a, b, c, d ) =
    [ a, b, c, d ]


type alias Place a =
    (a -> a) -> ( a, a, a, a ) -> ( a, ( a, a, a, a ) )


first : Place a
first fn ( a, b, c, d ) =
    ( a, ( fn a, b, c, d ) )


second : Place a
second fn ( a, b, c, d ) =
    ( b, ( a, fn b, c, d ) )


third : Place a
third fn ( a, b, c, d ) =
    ( c, ( a, b, fn c, d ) )


fourth : Place a
fourth fn ( a, b, c, d ) =
    ( d, ( a, b, c, fn d ) )


places : List (Place a)
places =
    [ first, second, third, fourth ]


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
            ( { model | currentGuess = snd <| place (\_ -> Just color) model.currentGuess }
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
        [ guessLogView solution guesses
        , currentGuesView currentGuess
        , guessButtonView
        , solutionView solution
        ]


guessLogView : Solution -> List Code -> Html.Html Msg
guessLogView solution guesses =
    Html.ul
        []
        (List.map (guessView solution) guesses)


guessView : Solution -> Code -> Html.Html Msg
guessView solution code =
    Html.li
        []
        ((List.map colorView (asList code))
            ++ [ hintView solution code ]
        )


hintView : Solution -> Code -> Html.Html Msg
hintView solution code =
    Html.text ((String.repeat (exactMatches solution code) "+") ++ (String.repeat (partialMatches solution code) "-"))


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


colorSelect : Place (Maybe Color) -> Maybe Color -> Html.Html Msg
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


exactMatches : Solution -> Code -> Int
exactMatches solution code =
    case solution of
        Unpicked ->
            0

        Picked solutionCode ->
            List.map2 (==) (asList solutionCode) (asList code)
                |> countTrue


partialMatches : Solution -> Code -> Int
partialMatches solution code =
    case solution of
        Unpicked ->
            0

        Picked solutionCode ->
            List.map (partialMatchesForColor solutionCode code) colors
                |> List.sum


partialMatchesForColor : Code -> Code -> Color -> Int
partialMatchesForColor solutionCode code color =
    if (List.member color (asList solutionCode)) then
        let
            exactMatches =
                exactMatchesForColor solutionCode code color
        in
            min (countColorsIn color code) (countColorsIn color solutionCode)
                |> (+) -exactMatches
    else
        0


countTrue : List Bool -> Int
countTrue bools =
    List.map
        (\bool ->
            if bool then
                1
            else
                0
        )
        bools
        |> List.sum


exactMatchesForColor : Code -> Code -> Color -> Int
exactMatchesForColor solutionCode code color =
    List.map2 (\color1 color2 -> (color == color1) && (color == color2)) (asList solutionCode) (asList code)
        |> countTrue


countColorsIn : Color -> Code -> Int
countColorsIn color code =
    List.map ((==) color) (asList code)
        |> countTrue
