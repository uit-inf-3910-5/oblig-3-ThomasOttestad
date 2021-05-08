namespace Shared

open System




type Position = char * int

type Sheet = Map<Position, string>




type Todo = {  
    Description: string 
    
} with 
    static member New = {
        Description = ""
    }

module Todo =

    let (|Integer|_|) (str: string) =
        let mutable intvalue = 0
        if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
        else None

    let isValidNumber (desc : string) = 
        match desc with
        // | Float desc -> true
        | Integer desc -> true
        | "\n" -> true
        | _ -> false

    let findNumbers =
        (fun (description : string) -> Seq.toList (description.Split ','))

    let isValid (description: string) : bool =

        let stringlist = findNumbers description

        match stringlist |> List.tryFind (fun x -> isValidNumber x = false) with
        | Some _ -> false
        | None -> true
        