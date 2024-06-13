port module Main exposing (main)

import Array
import Browser
import Html exposing (Html)
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
    = Shred { id : ShredID, name : String }


shredWith : Int -> String -> Shred
shredWith id name =
    Shred { id = ShredID id, name = name }


shredDecoder : D.Decoder Shred
shredDecoder =
    D.map2 shredWith
        (D.field "id" D.int)
        (D.field "name" D.string)


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
    | KillShred ShredID


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
            List.map (\txt -> Html.span [] [ Html.text txt ]) lines
    in
    Html.div [] (List.intersperse (Html.br [] []) spans)


viewShred : Shred -> Html Msg
viewShred (Shred data) =
    viewLines [ data.id |> asString, data.name ]


viewCisp : Result Problem Cisp -> Html Msg
viewCisp cisp =
    case cisp of
        Ok (Cisp shreds) ->
            Html.ol [] (List.map viewShred shreds)

        Err p ->
            Html.p [] [ Html.text (problemTostring p) ]


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

        KillShred id ->
            ( model, killShred id )


killShred : ShredID -> Cmd msg
killShred id =
    toCisp ("/cisp/kill " ++ asString id)


lsButton : Html.Html Msg
lsButton =
    Html.button [ Events.onClick ListShreds ] [ Html.text "List CISP shreds" ]


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.p [] [ Html.text "ELM >> 3334 >> CISP >>> 3333 >>> ELM" ]
        , lsButton
        , viewCisp model.status
        ]
