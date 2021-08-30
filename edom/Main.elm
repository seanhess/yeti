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
-- import Debug

-- TODO switch to forms, submit or submit1!
-- TODO try checkboxes, I want to do something on each click
-- TODO connect to actual backend


-- var currentUrl = window.location.pathname + window.location.search

  -- // TODO titles
  -- if (pageUrl != currentUrl) {
  --   let title = "Wookie Tab Title"
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
  | MissingParamsHeader
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
  { html : String
  , parsed : Result Error (Html Msg)
  , updates : Dict Action Value
  , requestId : RequestId
  , requestPending : Bool
  , url : Url
  , key : Key
  }

main : Program String Model Msg
main =
  Browser.application
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    , onUrlRequest = onUrlRequest
    , onUrlChange = UrlChange
    }

type Msg
  = ServerAction Action 
  | ServerUpdate Action Value
  | Loaded RequestId RequestType (Result Error (String, String))
  | UrlChange Url
  | None

subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none

onUrlRequest : UrlRequest -> Msg
onUrlRequest _ = None


-- "render" the existing html as it stands, and stand by for updates
init : String -> Url -> Key -> (Model, Cmd Msg)
init start url key =
  ( { html = start
    , updates = Dict.empty
    , parsed = parseHtml start
    , key = key
    , url = url
    , requestId = 0
    , requestPending = False
    }
  , Cmd.none
  )


onResponse : Response String -> Result Error (String, String)
onResponse response =
  case response of
    Http.GoodStatus_ meta body ->
      case Dict.get "x-params" meta.headers of
        Nothing -> Err MissingParamsHeader
        Just p -> Ok (p, body)

    Http.BadUrl_ _ ->
      Err <| ServerError BadUrl

    Http.Timeout_ ->
      Err <| ServerError Timeout

    Http.NetworkError_ ->
      Err <| ServerError NetworkError

    Http.BadStatus_ m b ->
      Err <| ServerError <| BadStatus m b
    


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
                   Browser.pushUrl model.key urlString
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

    None ->
      (model, Cmd.none)

nextRequestId : RequestId -> RequestId
nextRequestId = (+) 1

-- use the current url, but add the params
pageUrl : Url -> String -> String
pageUrl url params =
  Url.toString { url | query = Just <| "p=" ++ params }


view : Model -> Document Msg
view model =
  -- let test = div [] [ text "Elm Initialized" ]
  { title = "Titulo"
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
          MissingParamsHeader -> text "Missing Params from Server"
          ServerError BadUrl -> text "Bad url"
          ServerError Timeout -> text "Timeout"
          ServerError NetworkError -> text "Network Error"
          ServerError (BadStatus m b) -> text "Bad Status"
          CannotBuildUrl s -> text ("Bad Url Construction: " ++ s)
       ]
    ]
 





parseHtml : String -> Result Error (Html Msg)
parseHtml input = 
  case (Parser.run input) of
    Err _ -> Err FailedParse
    Ok nodes -> Ok <|
      div [ id "wookie-root-content"] <|
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
  let convertedAtts = List.map toAttribute atts
      convertedChilds = List.map toHtml childs
  in Html.node name convertedAtts convertedChilds

svgRoot : List Parser.Attribute -> List Parser.Node -> Html Msg
svgRoot atts childs =
  Svg.svg (List.map svgToAttribute atts) (List.map toSvg childs)

svgToAttribute : (AttributeName, AttributeValue) -> Svg.Attribute Msg
svgToAttribute (name, value) = VirtualDom.attribute name value

svgElement : ElementName -> List Parser.Attribute -> List Parser.Node -> Html Msg
svgElement name atts childs =
  let convertedAtts = List.map toAttribute atts
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
toAttribute : (AttributeName, AttributeValue) -> Html.Attribute Msg
toAttribute (name, value) =
  case name of
    "data-click" -> 
      Html.onClick (toMessage value)

    -- TODO, more complex. What does "update" mean in this context? How do we know it's an input field??

    "data-input" -> 
      Html.onInput (ServerUpdate value)

    "data-enter" -> 
      onEnter (toMessage value)

    "value" -> 
      Html.value value

    "checked" -> 
      Html.checked True

    _ ->
      Html.attribute name value


toMessage : AttributeValue -> Msg
toMessage val = ServerAction val


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
