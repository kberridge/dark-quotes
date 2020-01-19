open Tea
open Tea.App
open Tea.Html


type quote = {
  quote: string;
  attribution: string option;
  key: string;
}

type model = {
  currentQuote: quote option;
  failed: bool;
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
  [@@bs.deriving {accessors}]

let init() = 
  ( {
     currentQuote = None;
     failed = false
    }
  , Http.getString getQuoteUrl |> Http.send gotQuoteResponse
  )

let update (model : model) = function
  | GotQuoteResponse (Error _e) -> {currentQuote = None; failed = true}, Cmd.none
  | GotQuoteResponse (Ok data) -> 
    let open Json.Decoder in
    let quote_decoder = map3 (fun x y z -> {key = x; quote = y; attribution = z})
        (field "key" string)
        (field "quote" string)
        (field "attribution" string |> maybe) in
    begin match decodeString quote_decoder data with
      | Ok quote -> {currentQuote = Some quote; failed = false}
      | Error _ -> {currentQuote = None; failed = true}
    end, Cmd.none
  | GetDifferentQuote ->
    ( model
    , Http.getString (getDifferentQuoteUrl model.currentQuote) |> Http.send gotQuoteResponse
    )


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
    ; match model.currentQuote with
      | None -> noNode
      | Some _ -> 
        div
        [ style "display" "flex"
        ; style "justify-content" "center"
        ; style "padding-top" "2em"
        ]
        [ button [ onClick GetDifferentQuote ] [ text "Get a different quote"]
        ]
    ]

let main =
  standardProgram {
    init;
    update;
    view;
    subscriptions = (fun _ -> Sub.none)
  }