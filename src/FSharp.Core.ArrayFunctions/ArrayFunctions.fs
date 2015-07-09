namespace FSharp.Core.ArrayFunctions




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
        let arr = Array.zeroCreate  array2D.Length
        let rec loop x y acc =
            if   x = array2D.GetLength(0) then arr
            elif y = array2D.GetLength(1) then loop (x+1) 0 acc
            else arr.[acc] <- array2D.[x,y] 
                 loop x (y+1) (acc+1)
        loop 0 0 0


    let average (array2D:'a[,]) =
        checkNonNull "2D array" array2D
        ()
    
    let averageRow row (array2D:'a[,]) =
        checkNonNull "2D array" array2D
        ()


    let averageCol col (array2D:'a[,]) =
        checkNonNull "2D array" array2D
        ()

    let sumRows (array2D:'a[,]) = ()

    let sumCols (array2D:'a[,]) = ()

    let rev

    let revRows

    let revCols
    




        
