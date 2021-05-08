module Index

open Elmish

open Shared
open Feliz
open Feliz.Bulma
open Thoth.Json
open Thoth.Fetch
open System




type Model = { 
    Input : string
    Rows : int list
    Collums : char list
    Cells : Sheet
    SumInput : string
    Sum : int
    MultiInput : string
    Multi : int
    NewTodo : Todo option
}

type Msg = 
    // | CsvSave
    | SetInput of string
    | SetSumInput of string
    | SetMultiInput of string
    | Exn of System.Exception
    | DisplayCsv
    | HandleReset
    | CalculateSum
    | CalculateMulti
    | Init of Result<Todo,FetchError>
    | Save
    | SaveHandler of Result<Todo, FetchError>
    | ResetHandler of Result<Todo,FetchError>


// let getTodos () =
//     let decoder : Decoder<Todo list> = Thoth.Json.Decode.Auto.generateDecoder ()
//     Fetch.fetchAs (url="/api/getTodos", decoder = decoder)

// let addTodo (todo: Todo) =
//     let decoder : Decoder<Todo> = Thoth.Json.Decode.Auto.generateDecoder ()
//     Fetch.post (url="/api/addTodo", data = todo, decoder = decoder)

let saveCsv model =
    // let decoder : Decoder<string> = Thoth.Json.Decode.Auto.generateDecoder ()
    let decoder = Decode.Auto.generateDecoder<Todo> ()
    match model.NewTodo with
    | Some csv ->
        let p () = 
            promise {
                return! Fetch.tryPost (
                    "/api/model",
                    data = csv,
                    decoder = decoder
                )
            }
        model, Cmd.OfPromise.either p () SaveHandler Exn
    | None ->
            printfn " Save CSV not reached"
            model, Cmd.none

let init () : Model * Cmd<Msg> =
    let decoder = Decode.Auto.generateDecoder<Todo> ()
    let p () = 
        promise {
            return! Fetch.tryGet("/api/model", decoder = decoder) 
        }
    let model = { 
        Input = "" 
        Rows = [1..15]
        Collums = ['a'..'l']
        Cells = Map.empty
        SumInput = ""
        Sum = 0
        MultiInput = ""
        Multi = 0
        NewTodo = None
    }
    model, Cmd.OfPromise.either p () Init Exn

   

        
let splitRows =
    (fun (description : string) -> Seq.toList (description.Split "\n"))    

let splitLines =
    (fun (description : string) -> Seq.toList (description.Split ","))  

let translateCsv input model = 
    let rows = splitRows input
    let mutable temp = 1
    let mutable temp2 = 0

    let mutable map = Map.empty<Position, string>

    for row in rows do
        let line = splitLines row
        temp2 <- 0
        for number in line do
            if Todo.isValid number then
                map <- Map.add (model.Collums.Item(temp2),temp) number map
                temp2 <- temp2 + 1
        temp <- temp + 1
    map
    
let matchFind posC posR model =
    let opt = Map.tryFind (posC, posR) model.Cells
    match opt with
    | Some x -> x 
    | _ -> ""



let handleReset model = 
    let decoder = Decode.Auto.generateDecoder<Todo> ()
    let p () =
        promise { 
            return! Fetch.tryDelete(
                "/api/reset", 
                data = "",
                decoder = decoder
            ) 
        }
    
    let model = { 
        Input = "" 
        Rows = [1..15]
        Collums = ['a'..'l']
        Cells = Map.empty
        SumInput = ""
        Sum = 0
        MultiInput = ""
        Multi = 0
        NewTodo = None
        }    
    model, Cmd.OfPromise.either p () ResetHandler Exn

let sumRow model = 
    let mutable sum = 0
    let inputInt = model.SumInput |> int
    for letter in model.Collums do
        let cell = Map.tryFind (letter, inputInt) model.Cells
        match cell with
        | Some x -> sum <- sum + (x |> int)
        | _ -> printfn "no value"
    { model with Sum = sum }, Cmd.none

   
let sumCollum model = 
    let mutable sum = 0
    // let inputInt = model.SumInput |> int
    let charInput = model.SumInput |> char
    for numbers in model.Rows do
        let cell = Map.tryFind (charInput, numbers) model.Cells
        match cell with
        | Some x -> sum <- sum + (x |> int)
        | _ -> printfn "no value"
    { model with Sum = sum }, Cmd.none
        
let calcSum model =
    let inp = model.SumInput
    match System.Int32.TryParse inp with
    | true, num -> sumRow model
    | _, num -> sumCollum model


let multiRow model = 
    let mutable multi = 1
    let inputInt = model.MultiInput |> int
    for letter in model.Collums do
        let cell = Map.tryFind (letter, inputInt) model.Cells
        match cell with
        | Some x -> multi <- multi * (x |> int)
        | _ -> printfn "no value!!!!!!"
    { model with Multi = multi }, Cmd.none

let multiCollum model = 
    let mutable multi = 1
    let charInput = model.MultiInput |> char
    printfn "multiInput %A" model.MultiInput
    for numbers in model.Rows do
        let cell = Map.tryFind (charInput, numbers) model.Cells
        match cell with
        | Some x -> 
        multi <- multi * (x |> int)
        | _ -> printfn "no value"
    { model with Multi = multi }, Cmd.none

let calcMulti model =
    let inp = model.MultiInput
    match System.Int32.TryParse inp with
    | true, num -> multiRow model
    | _, num -> multiCollum model

let handleInit model =
    function
    | Ok s -> 
        let newCsv = translateCsv s.Description model
        { model with 
            Cells = newCsv 
        } , Cmd.none
    | Error err -> 
        printfn "Error in handle init %A" err 
        model, Cmd.none


let UpdateModelInput model (update : Todo -> Todo) = 
    let p = Option.defaultValue Todo.New model.NewTodo
    let p' = update p
    let model' = { model with 
                    NewTodo = Some p' 
                    Input = p'.Description 
                }
    model', Cmd.none 

let addCsv model =
    function
    | Ok p ->
        let map = translateCsv p.Description model
        { model with 
            Cells = map
            NewTodo = None

        }, Cmd.none
    | Error err -> 
        printfn "ERROR: addCsv (): %A" err
        model,Cmd.none


let reset (model : Model) =
    function
    | Ok s ->
        model, Cmd.none
    | Error e ->
        printfn "Error in reset %A " e
        model, Cmd.none 


let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | SetInput value -> UpdateModelInput model (fun p -> { p with Description = value })
    | SetSumInput value -> { model with SumInput = value }, Cmd.none 
    | SetMultiInput value -> { model with MultiInput = value }, Cmd.none 
    | DisplayCsv -> 
        let test = translateCsv model.Input model
        { model with 
            Cells = test 
            Input = ""}, Cmd.none
    | HandleReset -> handleReset model
    | CalculateSum -> calcSum model
    | CalculateMulti -> calcMulti model
    | Save -> saveCsv model
    | SaveHandler x -> addCsv model x
    | Init x -> handleInit model x
    | Exn e -> printfn "Exn %A " e ; model, Cmd.none
    | ResetHandler e -> reset model e

    
let containerBox model dispatch =        
    Bulma.box [
        Bulma.table [            
            Html.tr [
                Html.th [

                ]
                for collums in model.Collums do
                    Html.th [
                        prop.style [
                                style.fontSize 20
                                style.borderColor "black"
                                style.border(2, borderStyle.solid, color.black)
                        ]

                        
                        prop.text(string collums)
                    ]

            ]
            for rows in model.Rows do
                Html.tr [
                    Html.th [
                        prop.style [
                            style.fontSize 20
                            style.borderColor "black"
                            style.border(2, borderStyle.solid, color.black)
                        ]
                        prop.text(rows)
                    ]
                    for collums in model.Collums do
                    Html.th [
                        prop.style [
                            style.fontSize 20
                            style.borderColor "black"
                            style.border(2, borderStyle.solid, color.black)
                        ]
                        // Map.tryFind pos model.Cells
                        prop.text (matchFind collums rows model)

                    ]
                ]
        ]
        Bulma.button.a [
            color.isDanger
            prop.text "Reset"
            prop.onClick (fun _ -> dispatch HandleReset)
        ]

        Bulma.box [
            Html.p [
                prop.text "To sum or multiply a row input the number or letter that represents the row"
            ] 
            Html.input [
                prop.style [ 
                    style.width 150
                    style.marginTop 10
                    style.marginLeft 10
                    style.marginRight 20
                    ]
                //Må huske å sjekke etter valud input
                prop.value model.SumInput
                prop.placeholder ""
                prop.onChange (fun x -> SetSumInput x |> dispatch)

            ]
            Bulma.button.a [
                color.isDark
                prop.text "Sum"
                prop.onClick (fun _ -> dispatch CalculateSum)
            ]
            Html.input [
                prop.style [ 
                    style.width 100
                    style.marginTop 10
                    style.marginLeft 10
                    style.marginRight 20
                    // style.backgroundColor "red"
                    ]
                prop.value model.Sum
                prop.placeholder "Sum"

            ]
        ]

        // MULTI
        Bulma.box [
            Html.input [
                prop.style [ 
                    style.width 150
                    style.marginTop 10
                    style.marginLeft 10
                    style.marginRight 20
                    ]
                //Må huske å sjekke etter valud input
                prop.value model.MultiInput
                prop.placeholder ""
                prop.onChange (fun x -> SetMultiInput x |> dispatch)

            ]
            Bulma.button.a [
                color.isDark
                prop.text "Multiply"
                prop.onClick (fun _ -> dispatch CalculateMulti)
            ]
            Html.input [
                prop.style [ 
                    style.width 100
                    style.marginTop 10
                    style.marginLeft 10
                    style.marginRight 20
                    // style.backgroundColor "green"
                    ]
                prop.value model.Multi
                prop.placeholder ""

            ]
        ]




        Bulma.control.p [
            control.isExpanded

            prop.children [ 
               Bulma.textarea [
               prop.value model.Input
               prop.placeholder "Input Csv"
               prop.onChange (fun x -> SetInput x |> dispatch)
            ] ]
        ]
        Bulma.control.p [ Bulma.button.a [
            color.isPrimary
            prop.disabled ( Todo.isValid model.Input |> not)
            prop.onClick (fun _ -> dispatch Save )
            prop.text "Save"
        ] ]
    ]
       

let view (model: Model) (dispatch: Msg -> unit) =
    Bulma.container [
        prop.children [
            Bulma.title [
                // text.hasTextCentered
                prop.text "Excell"
            ] 
            containerBox model dispatch
        ]
    ]
    
