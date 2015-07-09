namespace FSharp.Core.ArrayFunctions
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
module Array2D =


    let inline checkNonNull argName arg = 
        match box arg with 
        | null -> nullArg argName 
        | _ -> ()
    

    let isEmpty (array2D:'a[,]) : bool  =
        checkNonNull "2D array" array2D
        (array2D.Length = 0)


    let empty<'a> = (Array2D.zeroCreate 0 0 ) : 'a [,]


    let flatten (array2D:'a[,]) : 'a [] =
        checkNonNull "2D array" array2D
        let xn,yn = array2D.GetLength(0)-1, array2D.GetLength(1)-1 
        let arr = Array.zeroCreate array2D.Length
        let mutable cnt = 0
        for x in 0..xn do
            for y in 0..yn do
                arr.[cnt] <- array2D.[x,y]            
                cnt <- cnt + 1
        arr

    let average (array2D:'a[,]) =
        checkNonNull "2D array" array2D
        ()
    
    let averageRow row (array2D:'a[,]) =
        checkNonNull "2D array" array2D
        ()


    let averageCol col (array2D:'a[,]) =
        checkNonNull "2D array" array2D
        ()

//    let sumRows (array2D:'a[,]) = ()
//    let sumCols (array2D:'a[,]) = ()
//    let rev
//    let revRows
//    let revCols
    


    let iter2   ( iterFunc: 'T1 -> 'T2 -> unit )
                ( arr1 : 'T1 [,] )
                ( arr2 : 'T2 [,] ) : unit =
        checkNonNull "2D array" arr1
        checkNonNull "2D array" arr2
        let x1_len, x2_len = arr1.GetLength(0),arr2.GetLength(0)
        let y1_len, y2_len = arr1.GetLength(1), arr2.GetLength(1)
        
        if x1_len <> x2_len then invalidArg "array2D" "Arrays had different lengths"
        if y1_len <> y2_len then invalidArg "array2D" "Arrays had different lengths"
        
        let rec recurse x y (arr1:'T1[,])(arr2:'T2[,]) =
            if   x = x1_len then ()
            elif y = y1_len then recurse (x+1) 0 arr1 arr2
            else iterFunc arr1.[x,y] arr2.[x,y]
                 recurse x (y+1) arr1 arr2
        recurse 0 0 arr1 arr2

    let iteri2( iterFunc: int -> int -> 'T1 -> 'T2 -> unit )
        ( arr1: 'T1 [,] )
        ( arr2: 'T2 [,] ) : unit =
        checkNonNull "2D array" arr1
        checkNonNull "2D array" arr2
        let x1_len, x2_len = arr1.GetLength(0),arr2.GetLength(0)
        let y1_len, y2_len = arr1.GetLength(1), arr2.GetLength(1)
        
        if x1_len <> x2_len then invalidArg "array2D" "Arrays had different lengths"
        if y1_len <> y2_len then invalidArg "array2D" "Arrays had different lengths"
        
        let rec recurse x y ( arr1: 'T1 [,] )( arr2:'T2[,] ) =
            printfn "x: %A" x
            printfn "y: %A" y
            printfn "x1_len: %A" x1_len
            if x = x1_len then ()
            elif y = y1_len then recurse (x+1) 0 arr1 arr2
            else iterFunc x y arr1.[x,y] arr2.[x,y]
                 recurse x ( y+1 ) arr1 arr2
        recurse 0 0 arr1 arr2