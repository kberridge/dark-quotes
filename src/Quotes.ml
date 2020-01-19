open Tea
open Tea.App
open Tea.Html


type quote = {
  quote: string;
  attribution: string option;
  key: string;
}

type addQuote = {
  quote: string;
  attribution: string;
}

type error = {
  errorKey: string;
  message: string;
}

type postQuoteResponse = {
  success: bool;
  quote: quote option;
  errors: error list option;
}

type model = {
  currentQuote: quote option;
  failed: bool;
  addQuote: addQuote;
  addFailed: bool;
  addQuotePending: bool;
  addQuoteErrors: error list;
}

let host = "https://kberridge-test.builtwithdark.com"
let getQuoteUrl = String.concat "" [host; "/quote"]
let getDifferentQuoteUrl quote =
  match quote with
  | Some q -> String.concat "" [host; "/quote"; "?not="; q.key ]
  | None -> getQuoteUrl

type msg = 
  | GotQuoteResponse of (string, string Http.error) Result.t
  | GetDifferentQuote
  | SetQuote of string
  | SetAttribution of string
  | PostQuote
  | GotPostQuoteResponse of (string, string Http.error) Result.t
  [@@bs.deriving {accessors}]

let apiPostQuote (addQuote : addQuote) =
  let open Json.Encoder in
  let body = object_
    [ "quote", string addQuote.quote
    ; "attribution", string addQuote.attribution
    ] 
    |> encode 0
  in
  Http.request
    { method' = "POST"
    ; headers = []
    ; url = String.concat "" [host; "/quote"]
    ; body = Web.XMLHttpRequest.StringBody body
    ; expect = Http.expectString
    ; timeout = None
    ; withCredentials = false
    }
  |> Http.send gotPostQuoteResponse

let init() = 
  ( {
     currentQuote = None;
     failed = false;
     addQuote = {
       quote = "";
       attribution = "";
     };
     addFailed = false;
     addQuotePending = false;
     addQuoteErrors = []
    }
  , Http.getString getQuoteUrl |> Http.send gotQuoteResponse
  )

let quote_decoder = 
  let open Json.Decoder in
  map3 (fun x y z -> {key = x; quote = y; attribution = z})
  (field "key" string)
  (field "quote" string)
  (field "attribution" string |> maybe)

let errors_decoder =
  let open Json.Decoder in
  let error_decoder = map2 (fun x y -> { errorKey = x; message = y; })
    (field "errorKey" string)
    (field "message" string)
  in
  Json.Decoder.list error_decoder

let updateCurrentQuoteFromJson model data =
  let open Json.Decoder in
  match decodeString quote_decoder data with
  | Ok quote -> {model with currentQuote = Some quote; failed = false}
  | Error _ -> {model with currentQuote = None; failed = true}

let update (model : model) = function
  | GotQuoteResponse (Error _e) -> {model with currentQuote = None; failed = true}, Cmd.none
  | GotQuoteResponse (Ok data) -> updateCurrentQuoteFromJson model data, Cmd.none
  | GetDifferentQuote ->
    ( model
    , Http.getString (getDifferentQuoteUrl model.currentQuote) |> Http.send gotQuoteResponse
    )
  | SetQuote q -> {model with addQuote = {model.addQuote with quote = q}}, Cmd.none
  | SetAttribution a -> {model with addQuote = {model.addQuote with attribution = a}}, Cmd.none
  | PostQuote -> {model with addQuotePending = true}, apiPostQuote model.addQuote
  | GotPostQuoteResponse (Error _e) -> {model with addFailed = true; addQuotePending = false}, Cmd.none
  | GotPostQuoteResponse (Ok data) -> 
    let open Json.Decoder in
    let response_decoder = Json.Decoder.map3 (fun a b c -> { success = a; quote = b; errors = c })
      (field "success" bool)
      (field "quote" quote_decoder |> maybe)
      (field "errors" errors_decoder |> maybe)
    in
    let responseResult = decodeString response_decoder data in
    match responseResult with
    | Ok response ->
      begin if response.success then
        match response.quote with
        | Some quote -> 
          { model with 
            currentQuote = Some quote; 
            failed = false; 
            addQuote = { quote = ""; attribution = "" }; 
            addQuotePending = false; 
            addQuoteErrors = []
          }, Cmd.none
        | None ->
          { model with failed = true; addQuotePending = false }, Cmd.none
      else
        match response.errors with
        | Some errors -> { model with addQuoteErrors = errors; addQuotePending = false }, Cmd.none
        | None -> { model with failed = true; addQuotePending = false }, Cmd.none
      end
    | Error _ -> {model with failed = true; addQuotePending = false}, Cmd.none

let viewQuote (quote: quote) =
  div
    []
    [ div
      []
      [ text quote.quote ]
    ; match quote.attribution with
      | Some attribution ->
        div
        [ style "padding-left" "2em" ]
        [ text (String.concat "" ["-"; attribution]) ]
      | None -> noNode
    ]

let viewErrorDisplayList (errors : error list) =
  div
    [ style "border" "1px solid red"
    ; style "padding" "1em"
    ]
    [ List.map (fun e -> li 
          [ style "color" "red "]
          [ text e.message ]) errors
      |> ul [ style "padding" "0 0 0 1em"; style "margin" "0" ]
    ]

let viewNewQuoteForm model =
  div
    [ style "display" "flex"
    ; style "justify-content" "center"
    ]
    [ div []
      [ h2 [ style "margin" "4em 0 0 0" ] [ text "Add a new quote" ]
      ; div 
          [ style "border" "1px solid black"
          ; style "padding" "1em"
          ; style "width" "20em"
          ]
          [ if List.length model.addQuoteErrors > 0 then
              viewErrorDisplayList model.addQuoteErrors
            else
              noNode
          ; label [ for' "quote"; style "display" "block" ] [ text "Quote" ]
          ; textarea [ name "quote"; style "width" "100%"; onChange setQuote; value model.addQuote.quote ] [ ]
          ; label [ for' "attribution"; style "display" "block" ] [ text "Attribution" ]
          ; input' [ name "attribution"; style "width" "100%"; onChange setAttribution; value model.addQuote.attribution ] [ ]
          ; button [ onClick PostQuote; style "margin-top" "1em"; Attributes.disabled model.addQuotePending ] [ text "Add Quote" ]
          ]
      ]
    ]

let view (model : model) =
  div
    []
    [ header
      [ style "text-align" "center" ]
      [ h1 [] [ text "Quotes, from Dark Lang!" ] ]
    ; div
      [ style "display" "flex"
      ; style "justify-content" "center"
      ]
      begin match model.failed with
      | true -> 
        [ div
            [ style "color" "red" ]
            [ text "Something has gone terribly, terribly wrong!" ]
        ]
      | false ->
        match model.currentQuote with
        | None -> [ text "fetching quote, hold on a sec..." ]
        | Some q -> 
          [ viewQuote q ]
      end
    ; begin match model.currentQuote with
      | Some _ -> 
        div
        [ style "display" "flex"
        ; style "justify-content" "center"
        ; style "padding-top" "2em"
        ]
        [ button [ onClick GetDifferentQuote ] [ text "Get a different quote"]
        ]
      | None -> noNode
      end
    ; viewNewQuoteForm model
    ; div
        [ style "display" "flex"
        ; style "justify-content" "center"
        ; style "padding-top" "2em"
        ]
        [ p []
          [ text "Built by "
          ; a [ href "https://www.kevinberridge.com"; target "_blank" ] [ text "Kevin Berridge" ]
          ; text " "
          ; a [ href "http://twitter.com/kberridge"; target "_blank" ] [ text "(@kberridge)"]
          ; text " in "
          ; a [ href "http://darklang.com"; target "_blank" ] [ text "Dark" ]
          ; text " and "
          ; a [ href "https://bucklescript.github.io/docs/en/stdlib-overview"; target "_blank" ] [ text "BuckleScript" ]
          ; text " and "
          ; a [ href "https://github.com/OvermindDL1/bucklescript-tea"; target "_blank" ] [ text "BuckleScript-TEA" ]
          ; text " "
          ; a [ href "https://github.com/kberridge/dark-quotes"; target "_blank" ] [text "(github)" ]
          ] 
        ]
    ]

let main =
  standardProgram {
    init;
    update;
    view;
    subscriptions = (fun _ -> Sub.none)
  }