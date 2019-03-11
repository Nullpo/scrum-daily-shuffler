import Browser
import Html as Keyed exposing (Html, text)
import Html.Attributes exposing (class, style)
import ListUtils exposing (shuffle, Seedoid(..))
import Task
import Time exposing (Posix)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid exposing (Column)
import Bootstrap.Grid.Row as Row
import Bootstrap.Button as Button




main = Browser.element
           { init = init
           , view = view
           , update = update
           , subscriptions = subscriptions
           }

subscriptions : Model -> Sub Msg
subscriptions model = Time.every 1000 Tick

names: List String
names = ["Ailan", "Chamo", "Leo", "Pablo"]

maxTime: Int
maxTime = 60 * 5

-- MODEL

type alias Model = {
                        shuffledItems: List String,
                        clockTicks: Int,
                        startedClock: Bool
                    }

init : () -> (Model, Cmd Msg)
init _ =  (Model [] (60 * 5) False, Cmd.none)

-- UPDATE

type Msg = Randomize | NewDate Posix | Tick Posix | StartClock

update : Msg -> Model-> (Model, Cmd Msg)
update msg model =
  case msg of
    Randomize ->
        (model, Task.perform NewDate Time.now)
    NewDate t->
        ({ model | shuffledItems = shuffle (IsPosix t) names}, Cmd.none)
    StartClock ->
        ({model | startedClock = True}, Cmd.none)
    Tick _ ->
        if model.startedClock then
            ({model | clockTicks = model.clockTicks - 1}, Cmd.none)
        else (model, Cmd.none)

-- VIEW

asRow: (Column Msg) -> (Html Msg)
asRow item = Grid.row [ Row.attrs [ style "margin-top" "1rem" ] ] [ item ]

asGrid: List (Html Msg) -> (Html Msg)
asGrid items = Grid.row [ Row.attrs [ style "margin-top" "1rem" ] ] (List.map asCol items)

asCol: Html Msg -> Column Msg
asCol item = asCols [item]

asCols: List (Html Msg) -> Column Msg
asCols item = Grid.col [] item

view : Model -> Html Msg
view model =
    Grid.container [ ]

        [
            CDN.stylesheet,
            viewContainer model
        ]

viewContainer: Model -> Html Msg
viewContainer = asRow << asCols << viewContent

viewContent : Model -> List (Html Msg)
viewContent model = [
                    asGrid [
                        viewRandomize,
                        viewStartClock
                    ],
                    asGrid [
                        (viewEntries model.shuffledItems),
                        (viewTimer model.startedClock model.clockTicks)
                    ]
                ]

-- TIMER

viewStartClock = Button.button [ Button.primary, Button.onClick StartClock ] [ text "Timer!" ]

viewTimer: Bool -> Int -> Html Msg
viewTimer hasStarted clock =
    case hasStarted of
        True -> Keyed.ul [] [text (String.fromInt clock)]
        False -> Keyed.ul [] [text "05:00:00"]

--- RANDOMIZE

viewRandomize: Html Msg
viewRandomize = Button.button [ Button.primary, Button.onClick Randomize ] [ text "Desordenar!" ]



viewEntries: List String -> Html Msg
viewEntries = Keyed.ul [ class "list-group" ] << List.map viewEntry

viewEntry: String -> Html Msg
viewEntry entry = Keyed.li [ class "list-group-item" ] [text entry]
