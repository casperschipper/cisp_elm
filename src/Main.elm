port module Main exposing (main)

import Array
import Browser
import Html exposing (..)
import Html.Attributes as Attr
import Html.Events as Events
import Json.Decode as D


port fromCisp : (D.Value -> msg) -> Sub msg


port toCisp : String -> Cmd msg


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch [ fromCisp CispStatus ]


type Shred
    = Shred { id : ShredID, name : String, idx : Int }


shredWith : Int -> String -> Int -> Shred
shredWith id name idx =
    Shred { id = ShredID id, name = name, idx = idx }


shredDecoder : D.Decoder Shred
shredDecoder =
    D.map3 shredWith
        (D.field "id" D.int)
        (D.field "name" D.string)
        (D.field "idx" D.int)


stateDecoder : D.Decoder Cisp
stateDecoder =
    D.list shredDecoder |> D.map Cisp


type ShredID
    = ShredID Int


asString : ShredID -> String
asString (ShredID id) =
    String.fromInt id


type alias Model =
    { status : Result Problem Cisp
    }


type alias Flags =
    ()


emptyCisp : Cisp
emptyCisp =
    Cisp []


init : Flags -> ( Model, Cmd msg )
init _ =
    ( { status = Ok emptyCisp }, Cmd.none )


type Msg
    = NoOp
    | CispStatus D.Value
    | ListShreds
    | KillShred Int


type alias Address =
    String


type Arg
    = I Int
    | F Float
    | S String


argToString : Arg -> String
argToString a =
    case a of
        I i ->
            "f " ++ String.fromInt i

        F f ->
            "i " ++ String.fromFloat f

        S s ->
            "s " ++ s


type OSC
    = OSC Address (Array.Array Arg)


arg : D.Decoder Arg
arg =
    D.oneOf
        [ D.int |> D.map I
        , D.float |> D.map F
        , D.string |> D.map S
        ]


type Cisp
    = Cisp (List Shred)


type Problem
    = WrongAddress String
    | JsonProblem D.Error
    | Other String


problemTostring : Problem -> String
problemTostring p =
    case p of
        WrongAddress s ->
            "wrong address - " ++ s

        JsonProblem e ->
            D.errorToString e

        Other str ->
            str


handleOSC : OSC -> Result Problem Cisp
handleOSC (OSC address args) =
    case address of
        "/cisp/state" ->
            case args |> Array.toList of
                (S json) :: [] ->
                    D.decodeString stateDecoder json |> Result.mapError JsonProblem

                _ ->
                    let
                        _ =
                            Debug.log "args" args
                    in
                    Result.Err (Other "wrong num args")

        _ ->
            Result.Err (Other ("wrong address: " ++ address))


viewLines : List String -> Html Msg
viewLines lines =
    let
        spans =
            List.map (\txt -> span [] [ text txt ]) lines
    in
    div [] (List.intersperse (br [] []) spans)


viewShred : Shred -> Html Msg
viewShred (Shred data) =
    div []
        [ viewLines [ data.id |> asString, data.name ]
        , stopShredButton (Shred data)
        ]


stopShredButton : Shred -> Html Msg
stopShredButton (Shred data) =
    button [ Events.onClick (KillShred data.idx) ] [ text "X" ]


viewCisp : Result Problem Cisp -> Html Msg
viewCisp cisp =
    case cisp of
        Ok (Cisp shreds) ->
            ol [] (List.map viewShred shreds)

        Err p ->
            Html.p [] [ text (problemTostring p) ]


osc : D.Decoder OSC
osc =
    D.array arg
        |> D.andThen
            (\arr ->
                case arr |> Array.toList of
                    [] ->
                        D.fail "kid, this aint that type of json"

                    (S address) :: _ ->
                        D.succeed (OSC address (Array.slice 1 (Array.length arr) arr))

                    _ ->
                        D.fail "expected address as first element of array"
            )


printOsc : OSC -> String
printOsc o =
    case o of
        OSC address args ->
            String.join "\n" ([ "address: ", address, "args: " ] ++ (args |> Array.map argToString |> Array.toList))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        CispStatus status ->
            let
                state =
                    D.decodeValue osc status |> Result.mapError JsonProblem |> Result.andThen handleOSC
            in
            ( { model
                | status = state
              }
            , Cmd.none
              --toCisp "hi cisp, elm here"
            )

        ListShreds ->
            ( model, toCisp "/cisp/ls" )

        KillShred i ->
            ( model, killShred i )


killShred : Int -> Cmd msg
killShred idx =
    toCisp ("/cisp/stopindex " ++ String.fromInt idx)


lsButton : Html Msg
lsButton =
    button [ Events.onClick ListShreds ] [ text "List CISP shreds" ]


view : Model -> Html Msg
view model =
    div []
        [ p [] [ text "ELM >> 3334 >> CISP >>> 3333 >>> ELM" ]
        , lsButton
        , viewCisp model.status
        ]
