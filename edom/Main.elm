module Main exposing (..)

import Browser exposing (UrlRequest, Document)
import Browser.Navigation as Browser exposing (Key)
import Url exposing (Url)
import Html exposing (Html, div, text, node)
import Html.Attributes as Html exposing (id)
import Html.Events as Html
import Html.Parser as Parser exposing (Node(..))
import Http exposing (Response)
import Dict exposing (Dict)
-- import Debug

-- TODO switch to forms, submit or submit1!
-- TODO try checkboxes, I want to do something on each click
-- TODO connect to actual backend


-- let pageUrl = res.headers.get('X-Page-Url')
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

type ServerError
  = Error

type alias Model =
  { html : String
  , inputs : Dict Id Value
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
  = Input Id Value
  | ServerAction String 
  | Loaded (Result ServerError (Maybe String, String))
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
    , inputs = Dict.empty
    , key = key
    , url = url
    }
  , Cmd.none
  )


onResponse : Response String -> Result ServerError (Maybe String, String)
onResponse response =
  case response of
    Http.GoodStatus_ meta body ->
      Ok (Dict.get "x-page-url" meta.headers, body)

    Http.BadUrl_ _ ->
      Err Error

    Http.Timeout_ ->
      Err Error

    Http.NetworkError_ ->
      Err Error

    Http.BadStatus_ _ _ ->
      Err Error
    


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ServerAction action ->
      (model
      , Http.request
        { method = "POST"
        , headers = [Http.header "accept" "application/vdom"]
        , url = Url.toString model.url
        , body = Http.stringBody "text/plain" action
        , timeout = Nothing
        , tracker = Nothing
        , expect = Http.expectStringResponse Loaded onResponse
        }
      )

    Loaded (Ok (pageUrl, content)) ->
      ( { model | html = content }
      , case pageUrl of
          Nothing -> Cmd.none
          Just u -> Browser.pushUrl model.key u
      )

    Loaded (Err _) ->
      (model, Cmd.none)

    Input id value ->
      ({ model | inputs = Dict.insert id value model.inputs }
      , Cmd.none
      )

    UrlChange url ->
      ( { model | url = url }
      , Cmd.none
      )

    None ->
      (model, Cmd.none)



view : Model -> Document Msg
view model =
  -- let test = div [] [ text "Elm Initialized" ]
  { title = "Titulo", body = [viewHtml model] }

viewHtml : Model -> Html Msg
viewHtml model = 
  case (Parser.run model.html) of
    Err _ -> div [] [ text "ERR" ]
    Ok nodes -> 
      div [ id "wookie-root-content"] <|
        List.map toHtml nodes

  -- div []
  --   [ button [ onClick Load ] [ text "Load" ]
  --   , div [] [ text model.html ]
    -- ]



type alias ElementName = String
type alias AttributeName = String
type alias AttributeValue = String


toHtml : Node -> Html Msg
toHtml node =
  case node of
    (Text s) ->
      text s
    (Comment _) ->
      text ""
    (Element name atts childs) ->
      toElement name atts childs



toElement : ElementName -> List Parser.Attribute -> List Parser.Node -> Html Msg
toElement name atts childs =
  let convertedAtts = List.map toAttribute atts
      convertedChilds = List.map toHtml childs
  in case (name, idFromAttributes atts) of
    ("input", Just id) ->
      Html.node name (inputListener id :: convertedAtts) convertedChilds
    _ -> 
      Html.node name convertedAtts convertedChilds


-- IF you have an id attribute, yes
inputListener : String -> Html.Attribute Msg
inputListener id =
    Html.onInput (Input id)

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
      Html.onClick (ServerAction value)
    "checked" -> 
      Html.checked True
    _ ->
      Html.attribute name value
