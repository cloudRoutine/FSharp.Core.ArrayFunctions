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
    

    let private loop (func:int->int->'a[,]->'b[,]->unit) (array2D:'a[,]) : 'b[,] =
        let x_len = array2D.GetLength(0) 
        let y_len = array2D.GetLength(1)         
        let outarr = Array2D.zeroCreate x_len y_len :'b[,]
        let rec recurse x y (array2D:'a[,]) =
            if   x = x_len then outarr
            elif y = y_len then recurse (x+1) 0 array2D
            else func x y array2D outarr
                 recurse x (y+1) array2D
        recurse 0 0 array2D


    let private loopAcc (func:int->int->'acc->'a[,]->'b[,]->'acc) (acc:'acc) (array2D:'a[,]) : 'b[,] =
        let x_len = array2D.GetLength(0) 
        let y_len = array2D.GetLength(1)         
        let outarr = Array2D.zeroCreate x_len y_len :'b[,]
        let rec recurse x y acc (array2D:'a[,]) =
            if   x = x_len then outarr
            elif y = y_len then recurse (x+1) 0 acc array2D
            else recurse x (y+1) (func x y acc array2D outarr) array2D
        recurse 0 0 acc array2D


    let private loopInto (func:int->int->'col->'a[,]->'col) (col:'col) (array2D:'a[,]) : 'col =      
        let rec recurse x y col (array2D:'a[,]) =
            if   x = array2D.GetLength(0)  then col
            elif y = array2D.GetLength(1) then recurse (x+1) 0 col array2D
            else recurse x (y+1) (func x y col array2D) array2D
        recurse 0 0 col array2D


    let private loopAccInto (func:int->int->'acc*'col->'a[,]->'acc*'col) 
                            (acc:'acc) (col:'col) (array2D:'a[,])       : 'col =     
        let rec recurse x y (acc,col) (array2D:'a[,]) =
            if   x = array2D.GetLength(0) then col
            elif y = array2D.GetLength(1) then recurse (x+1) 0 (acc,col) array2D
            else recurse x (y+1) (func x y (acc,col) array2D) array2D
        recurse 0 0 (acc,col) array2D




    let flatten (array2D:'a[,]) : 'a [] =
        checkNonNull "2D array" array2D
        let func x y ((acc,arr):int*'a[]) (array2D:'a[,]) = 
            arr.[acc] <- array2D.[x,y]; (acc+1,arr)
        loopAccInto func 0 (Array.zeroCreate  array2D.Length) array2D                    


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