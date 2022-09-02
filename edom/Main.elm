port module Main exposing (..)

import VirtualDom
import Browser exposing (UrlRequest, Document)
import Browser.Navigation as Browser exposing (Key)
import Url exposing (Url)
import Html exposing (Html, div, text, node, Attribute, span)
import Svg exposing (Svg)
import Html.Attributes as Html exposing (id)
import Html.Events as Events
import Html.Parser as HParser exposing (Node(..))
import Parser exposing (DeadEnd, deadEndsToString)
import Process
import Task
import String
import Http exposing (Response)
import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Decode as Decode

type alias Id = String
type alias Value = String

type Error
  = FailedParse (List DeadEnd)
  | MissingHeader String
  | ServerError ServerError
  | CannotBuildUrl String

type ServerError
  = BadUrl
  | Timeout
  | NetworkError
  | BadStatus Http.Metadata String


type alias Action = String


type RequestType
  = RequestAction
  | RequestLoadUrl

type alias RequestId = Int


port sendEvent : String -> Cmd msg


updateDOM : Cmd Msg
updateDOM =
  Process.sleep 5 |> Task.perform (always (Updated ()))

type alias Model =
  { html : Body
  , title : Title 
  , state : State
  , parsed : Result Error (Html Msg)
  , updates : Dict Action Value
  , requestId : RequestId
  , requestPending : Bool
  , url : Url
  , key : Key
  }

main : Program (Title, Body, State) Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChange
    }

type Msg
  = ServerAction Action 
  | ServerUpdate Action Value
  | Loaded RequestId RequestType (Result Error (Params, Body))
  | UrlChange Url
  | LinkClicked UrlRequest
  | Updated ()

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- "render" the existing html as it stands, and stand by for updates
-- no... I want the state from the request NOW. How can I get it? Passed in from the JS?
init : (Title, Body, State) -> Url -> Key -> (Model, Cmd Msg)
init (title, start, state) url key =
  ( { html = start
    , title = title
    , state = state
    , updates = Dict.empty
    , parsed = parseHtml start
    , key = key
    , url = url
    , requestId = 0
    , requestPending = False
    }
  , updateDOM
  )



type alias Body = String
type alias Params = String
type alias State = String
type alias Title = String
type alias Content = String

onResponse : Response String -> Result Error (Params, Body)
onResponse response =
  case response of
    Http.GoodStatus_ meta body ->
      case (getHeader "x-params" meta.headers) of
        (Ok p) -> Ok (String.replace "%20" "+" p, body)
        (Err e) -> Err e

    Http.BadUrl_ _ ->
      Err <| ServerError BadUrl

    Http.Timeout_ ->
      Err <| ServerError Timeout

    Http.NetworkError_ ->
      Err <| ServerError NetworkError

    Http.BadStatus_ m b ->
      Err <| ServerError <| BadStatus m b

getHeader : String -> Dict String String -> Result Error String
getHeader h heads =
  case Dict.get h heads of
    Nothing -> Err (MissingHeader h)
    Just p -> Ok p

    


serializeValueAction : Action -> Value -> String
serializeValueAction act val =
  -- this assumes act is an unapplied haskell constructor
  -- assumes action wants a string
  (act ++ " " ++ Encode.encode 0 (Encode.string val))

serializeChangeAction : Action -> String -> Action
serializeChangeAction act inp =
  -- this assumes act is an unapplied haskell constructor
  -- assumed input is shown haskell code as well
  (act ++ " " ++ inp)

requestBody : Action -> Model -> Body
requestBody action model =
  let updates = Dict.foldl (\act val items -> serializeValueAction act val :: items) [] model.updates
      body = String.join "\n" (model.state :: updates ++ [action])
  in body

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    ServerUpdate action value ->
      ( { model | updates = Dict.insert action value model.updates }
      , Cmd.none
      )

    ServerAction action ->
      let rid = nextRequestId model.requestId
      in if Debug.log "ServerAction" <| model.requestPending
            then ( model, Cmd.none )
            else ( { model | updates = Dict.empty, requestId = rid, requestPending = True }
                 , Http.request
                     { method = "POST"
                     , headers = [Http.header "accept" "application/vdom"]
                     , url = Url.toString model.url
                     , body = Http.stringBody "text/plain" (requestBody action model)
                     , timeout = Nothing
                     , tracker = Nothing
                     , expect = Http.expectStringResponse (Loaded rid RequestAction) onResponse
                     }
                 )

    Loaded _ rt (Ok (params, body)) ->
      let urlString = pageUrl model.url params
          (state, content) = parseBody body
          updateUrl = case rt of
            RequestAction ->
              if Url.fromString urlString /= Just model.url
                then Browser.pushUrl model.key urlString
                else Cmd.none
            RequestLoadUrl ->
              Cmd.none


      in case Url.fromString urlString of
          Nothing ->
            ( { model | parsed = Err (CannotBuildUrl urlString)
                      , requestPending = False
              }
            , updateDOM
            )
          Just url -> 
            ( { model | html = content
                      , parsed = parseHtml content
                      , url = url
                      , state = state
                      , requestPending = False
              }
            , Cmd.batch [updateUrl, updateDOM]
            )

    Loaded _ _ (Err e) ->
      ( { model | parsed = Err e}
      , updateDOM
      )

    UrlChange url ->
      let rid = nextRequestId model.requestId
      in if model.url == url || model.requestPending
        then ( model, Cmd.none )
        else ( { model | url = url, requestId = rid, requestPending = True }
             , Http.request
                { method = "GET"
                , headers = [Http.header "accept" "application/vdom"]
                , url = Url.toString url
                , body = Http.stringBody "text/plain" ""
                , timeout = Nothing
                , tracker = Nothing
                , expect = Http.expectStringResponse (Loaded rid RequestLoadUrl) onResponse
                }
             )

    -- an actual link trigger by the user interacting with the browser
    LinkClicked urlRequest ->
      case urlRequest of
        Browser.Internal url ->
          ( model, Browser.pushUrl model.key (Url.toString url) )

        Browser.External href ->
          ( model, Browser.load href )

    Updated _ ->
      (model, sendEvent "updateDOM")

parseBody : Body -> (State, Content)
parseBody b = 
  let ls = String.lines b
      st = String.join "" <| List.take 1 ls
      ct = String.join "\n" <| List.drop 1 ls
  in (st, ct)

nextRequestId : RequestId -> RequestId
nextRequestId = (+) 1

-- use the current url, but add the params
pageUrl : Url -> String -> String
pageUrl url params =
  Url.toString { url | query = Just params }


view : Model -> Document Msg
view model =
  { title = model.title
  , body =
      [ case model.parsed of
          Ok content -> content
          Err e -> viewError e
      ]
  }

viewError : Error -> Html Msg
viewError err =
  div []
    [ span [] [ text "Error: " ]
    , span []
       [ case err of
          FailedParse ds -> text <| "Failed Parse: " ++ deadEndsToString ds
          MissingHeader h -> text ("Missing Header from Server: " ++ h)
          ServerError BadUrl -> text "Bad url"
          ServerError Timeout -> text "Timeout"
          ServerError NetworkError -> text "Network Error"
          ServerError (BadStatus _ _) -> text "Bad Status"
          CannotBuildUrl s -> text ("Bad Url Construction: " ++ s)
       ]
    ]
 





parseHtml : String -> Result Error (Html Msg)
parseHtml input = 
  case (HParser.run input) of
    Err ld -> Err (FailedParse ld)
    Ok nodes -> Ok <|
      div [ id "juniper-root-content"] <|
        List.map toHtml nodes



type alias ElementName = String
type alias AttributeName = String
type alias AttributeValue = String


toHtml : HParser.Node -> Html Msg
toHtml node =
  case node of
    (Text s) ->
      text s
    (Comment _) ->
      text ""
    (Element "svg" atts childs) ->
      svgRoot atts childs
    (Element name atts childs) ->
      toElement name atts childs

toSvg : HParser.Node -> Svg Msg
toSvg node =
  case node of
    (Text s) ->
      text s
    (Comment _) ->
      text ""
    (Element name atts childs) ->
      svgElement name atts childs




toElement : ElementName -> List HParser.Attribute -> List HParser.Node -> Html Msg
toElement name atts childs =
  let convertedAtts = List.concatMap toAttribute atts
      convertedChilds = List.map toHtml childs
  in Html.node name convertedAtts convertedChilds

svgRoot : List HParser.Attribute -> List HParser.Node -> Html Msg
svgRoot atts childs =
  Svg.svg (List.map svgToAttribute atts) (List.map toSvg childs)

svgToAttribute : (AttributeName, AttributeValue) -> Svg.Attribute Msg
svgToAttribute (name, value) = VirtualDom.attribute name value

svgElement : ElementName -> List HParser.Attribute -> List HParser.Node -> Html Msg
svgElement name atts childs =
  let convertedAtts = List.concatMap toAttribute atts
      convertedChilds = List.map toHtml childs
  in Svg.node name convertedAtts convertedChilds
  

-- IF you have an id attribute, yes
-- inputListener : String -> Html.Attribute Msg
-- inputListener id =
--     Html.onInput (Input id)

idFromAttributes : List HParser.Attribute -> Maybe String
idFromAttributes atts =
  case List.filter (\(name, _) -> name == "id") atts of
    [] -> Nothing
    ((_,id)::_) -> Just id


toAttribute : (AttributeName, AttributeValue) -> List (Html.Attribute Msg)
toAttribute (name, value) =
  case name of
    "data-on-click" -> 
      [Events.onClick (ServerAction value)]

    "data-on-input" -> 
      -- automatically commit changes on enter or blur for inputs
      [Events.onInput (ServerUpdate value), onEnter (ServerAction submit), Events.onBlur (ServerAction submit)]

    "data-on-select" -> 
      [Events.onInput (\s -> ServerAction (serializeChangeAction value s))]

    "data-on-enter" -> 
      [onEnter (ServerAction value)]

    "data-on-delete" -> 
      [onDelete (\s -> ServerAction (serializeValueAction value s))]

    "value" -> 
      [Html.value value]

    "checked" -> 
      [Html.checked True]

    _ ->
      [Html.attribute name value]


onDelete : (String -> msg) -> Attribute msg
onDelete toMsg =
  (Events.on "delete"
      (Decode.field "value" Decode.string
          |> Decode.map toMsg
      )
  )

alwaysStop : a -> (a, Bool)
alwaysStop x =
  (x, True)

submit : AttributeValue
submit = "|Submit|"

onEnter : msg -> Attribute msg
onEnter msg =
  (Events.on "keydown"
      (Decode.field "key" Decode.string
          |> Decode.andThen
              (\key ->
                  if key == "Enter" then
                      Decode.succeed msg

                  else
                      Decode.fail "Not the enter key"
              )
      )
  )
