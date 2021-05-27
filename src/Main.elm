module Main exposing (..)

import Browser exposing (..)
import Html exposing (Html, text)
import Html.Attributes as Attributes exposing (style)
import Html.Events as Events
import Http exposing (..)
import Json.Decode exposing (Decoder, field, list)
import Json.Encode
import Maybe exposing (..)
import Result exposing (..)



---- MODEL ----


type alias Model =
    { loading : Bool
    , error : Maybe String
    , todoItemList : List TodoItem
    }


type alias TodoItem =
    { id : Int
    , title : String
    }


init : () -> ( Model, Cmd Msg )
init flags =
    ( { loading = True
      , error = Nothing
      , todoItemList = []
      }
    , request
        { method = "GET"
        , headers = []
        , url = "https://jsonplaceholder.typicode.com/todos"
        , body = Http.emptyBody
        , expect = Http.expectJson FetchedTodoItems (Json.Decode.list todoItemDecoder)
        , timeout = Nothing
        , tracker = Nothing
        }
    )


todoItemDecoder : Decoder TodoItem
todoItemDecoder =
    Json.Decode.map2 TodoItem
        (Json.Decode.field "id" Json.Decode.int)
        (field "title" Json.Decode.string)



---- UPDATE ----


type Msg
    = FetchedTodoItems (Result Http.Error (List TodoItem))
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FetchedTodoItems tmp ->
            ( case tmp of
                Err error ->
                    { loading = False, error = Just "Something went wrong ...", todoItemList = [] }

                Ok value ->
                    { model
                        | loading = False
                        , error = Nothing
                        , todoItemList = value
                    }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    if model.loading then
        Html.text "Loading ..."

    else
        case model.error of
            Just error ->
                Html.text error

            Nothing ->
                todoItemListView model.todoItemList


todoItemListView todoItemList =
    Html.div
        [ Attributes.style "flex-direction" "column"
        , Attributes.style "display" "flex"
        , Attributes.style "align-items" "center"
        ]
        [ Html.h1 [] [ text "Todo list app!" ]
        , todoItemList
            |> List.map (\todoItem -> formatTodoListItem todoItem)
            |> Html.div
                [ style "flex-direction" "column"
                , style "display" "flex"
                , style "align-items" "center"
                ]
        ]


formatTodoListItem { id, title } =
    Html.div [] [ Html.text (String.fromInt id ++ " - " ++ title) ]



---- PROGRAM ----


main : Program () Model Msg
main =
    element
        { view = view
        , init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        }
