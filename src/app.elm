import Browser
import Html as Keyed exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import ListUtils exposing (shuffle, Seedoid(..))
import Random
import Task
import Time exposing (Posix, posixToMillis)


main = Browser.element
           { init = init
           , view = view
           , update = update
           , subscriptions = subscriptions
           }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

names: List String
names = ["Ailan", "Chamo", "Leo", "Pablo"]

-- MODEL

type alias Model = List String

init : () -> (Model, Cmd Msg)
init _ =  ([], Cmd.none)

-- UPDATE

type Msg = Randomize | NewDate Posix

update : Msg -> Model-> (Model, Cmd Msg)
update msg model =
  case msg of
    Randomize ->
        (model, Task.perform NewDate Time.now)
    NewDate t->
        (shuffle (IsPosix t) names, Cmd.none)

-- VIEW

view : Model -> Html Msg
view model =
      div [] [
        viewRandomize,
        viewEntries model
      ]

viewRandomize: Html Msg
viewRandomize = button [ onClick Randomize ] [ text "Desordenar!" ]

viewEntries: List String -> Html Msg
viewEntries = Keyed.ul [ ] << List.map viewEntry

viewEntry: String -> Html Msg
viewEntry entry = div [] [text entry]
