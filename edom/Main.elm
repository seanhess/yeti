module Main exposing (..)

import VirtualDom
import Browser exposing (UrlRequest, Document)
import Browser.Navigation as Browser exposing (Key)
import Url exposing (Url)
import Html exposing (Html, div, text, node, Attribute, span)
import Svg exposing (Svg)
import Html.Attributes as Html exposing (id)
import Html.Events as Html
import Html.Parser as Parser exposing (Node(..))
import Http exposing (Response)
import Dict exposing (Dict)
import Json.Encode as Encode
import Json.Decode as Decode


-- TODO switch to forms, submit or submit1!
-- TODO try checkboxes, I want to do something on each click
-- TODO connect to actual backend

-- var currentUrl = window.location.pathname + window.location.search

  -- // TODO titles
  -- if (pageUrl != currentUrl) {
  --   let title = "Juniper Tab Title"
  --   console.log(" - ", "pageUrl", pageUrl)
  --   window.history.pushState({pageUrl: pageUrl}, title, pageUrl)
  -- }

    -- method: "POST",
    -- headers: {"Accept": "application/vdom"},
    -- body: body

type alias Id = String
type alias Value = String

type Error
  = FailedParse
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


type alias Model =
  { html : Body
  , title : Title 
  , parsed : Result Error (Html Msg)
  , updates : Dict Action Value
  , requestId : RequestId
  , requestPending : Bool
  , url : Url
  , key : Key
  }

main : Program (Title, Body) Model Msg
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

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none


-- "render" the existing html as it stands, and stand by for updates
init : (Title, Body) -> Url -> Key -> (Model, Cmd Msg)
init (title, start) url key =
  ( { html = start
    , title = title
    , updates = Dict.empty
    , parsed = parseHtml start
    , key = key
    , url = url
    , requestId = 0
    , requestPending = False
    }
  , Cmd.none
  )



type alias Body = String
type alias Params = String
type alias Title = String

onResponse : Response String -> Result Error (Params, Body)
onResponse response =
  case response of
    Http.GoodStatus_ meta body ->
      case (getHeader "x-params" meta.headers) of
        (Ok p) -> Ok (p, body)
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
  (act ++ " " ++ Encode.encode 0 (Encode.string val))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    ServerUpdate action value ->
      ( { model | updates = Dict.insert action value model.updates }
      , Cmd.none
      )

    ServerAction action ->
      let updates = Dict.foldl (\act val items -> serializeValueAction act val :: items) [] model.updates
          body = String.join "\n" (updates ++ [action])
          rid = nextRequestId model.requestId
      in if model.requestPending
            then ( model, Cmd.none )
            else ( { model | updates = Dict.empty, requestId = rid, requestPending = True }
                 , Http.request
                     { method = "POST"
                     , headers = [Http.header "accept" "application/vdom"]
                     , url = Url.toString model.url
                     , body = Http.stringBody "text/plain" body
                     , timeout = Nothing
                     , tracker = Nothing
                     , expect = Http.expectStringResponse (Loaded rid RequestAction) onResponse
                     }
                 )

    Loaded _ rt (Ok (params, content)) ->
      let urlString = pageUrl model.url params
      in case Url.fromString urlString of
          Nothing -> ( { model | parsed = Err (CannotBuildUrl urlString), requestPending = False }, Cmd.none )
          Just url -> 
            ( { model | html = content, parsed = parseHtml content, url = url, requestPending = False }
            ,  case rt of
                 RequestAction ->
                   if Url.fromString urlString /= Just model.url
                     then Browser.pushUrl model.key urlString
                     else Cmd.none
                 RequestLoadUrl ->
                  Cmd.none
            )

    Loaded _ _ (Err e) ->
      ( { model | parsed = Err e}
      , Cmd.none
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

    -- None ->
    --   (model, Cmd.none)

nextRequestId : RequestId -> RequestId
nextRequestId = (+) 1

-- use the current url, but add the params
pageUrl : Url -> String -> String
pageUrl url params =
  Url.toString { url | query = Just <| "p=" ++ params }


view : Model -> Document Msg
view model =
  -- let test = div [] [ text "Elm Initialized" ]
  { title = model.title
  , body =
      [ case model.parsed of
          Ok content -> content
          Err e -> viewError e
      ]
  }

viewError : Error -> Html Msg
viewError e =
  div []
    [ span [] [ text "Error: " ]
    , span []
       [ case e of
          FailedParse -> text "Failed Parse"
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
  case (Parser.run input) of
    Err _ -> Err FailedParse
    Ok nodes -> Ok <|
      div [ id "juniper-root-content"] <|
        List.map toHtml nodes



type alias ElementName = String
type alias AttributeName = String
type alias AttributeValue = String


toHtml : Parser.Node -> Html Msg
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

toSvg : Parser.Node -> Svg Msg
toSvg node =
  case node of
    (Text s) ->
      text s
    (Comment _) ->
      text ""
    (Element name atts childs) ->
      svgElement name atts childs




toElement : ElementName -> List Parser.Attribute -> List Parser.Node -> Html Msg
toElement name atts childs =
  let convertedAtts = List.concatMap toAttribute atts
      convertedChilds = List.map toHtml childs
  in Html.node name convertedAtts convertedChilds

svgRoot : List Parser.Attribute -> List Parser.Node -> Html Msg
svgRoot atts childs =
  Svg.svg (List.map svgToAttribute atts) (List.map toSvg childs)

svgToAttribute : (AttributeName, AttributeValue) -> Svg.Attribute Msg
svgToAttribute (name, value) = VirtualDom.attribute name value

svgElement : ElementName -> List Parser.Attribute -> List Parser.Node -> Html Msg
svgElement name atts childs =
  let convertedAtts = List.concatMap toAttribute atts
      convertedChilds = List.map toHtml childs
  in Svg.node name convertedAtts convertedChilds
  

-- IF you have an id attribute, yes
-- inputListener : String -> Html.Attribute Msg
-- inputListener id =
--     Html.onInput (Input id)

idFromAttributes : List Parser.Attribute -> Maybe String
idFromAttributes atts =
  case List.filter (\(name, _) -> name == "id") atts of
    [] -> Nothing
    ((_,id)::_) -> Just id


-- TODO: parse data-click, etc
toAttribute : (AttributeName, AttributeValue) -> List (Html.Attribute Msg)
toAttribute (name, value) =
  case name of
    "data-click" -> 
      [Html.onClick (ServerAction value)]

    -- TODO, more complex. What does "update" mean in this context? How do we know it's an input field??

    "data-input" -> 
      [Html.onInput (ServerUpdate value), onEnter (ServerAction submit), Html.onBlur (ServerAction submit)]

    -- "data-enter" -> 
    --   [onEnter (toMessage value)]

    -- "data-blur" -> 
    --   [Html.onBlur (toMessage value)]

    "value" -> 
      [Html.value value]

    "checked" -> 
      [Html.checked True]

    _ ->
      [Html.attribute name value]


submit : AttributeValue
submit = "|Submit|"

onEnter : msg -> Attribute msg
onEnter msg =
  (Html.on "keydown"
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
