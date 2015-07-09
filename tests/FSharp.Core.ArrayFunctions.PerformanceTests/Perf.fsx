#r @"..\..\packages\PerfUtil\lib\net40\PerfUtil.dll"



open PerfUtil


let inline checkNonNull argName arg = 
    match box arg with 
    | null -> nullArg argName 
    | _ -> ()
    

let loopAccInto (func:int->int->'acc*'col->'a[,]->'acc*'col) 
                        (acc:'acc) (col:'col) (array2D:'a[,])       : 'col =     
    let rec recurse x y (acc,col) (array2D:'a[,]) =
        if   x = array2D.GetLength(0) then col
        elif y = array2D.GetLength(1) then recurse (x+1) 0 (acc,col) array2D
        else recurse x (y+1) (func x y (acc,col) array2D) array2D
    recurse 0 0 (acc,col) array2D




let flatten1 (array2D:'a[,]) : 'a [] =
    checkNonNull "2D array" array2D
    let func x y ((acc,arr):int*'a[]) (array2D:'a[,]) = 
        arr.[acc] <- array2D.[x,y]; (acc+1,arr)
    loopAccInto func 0 (Array.zeroCreate  array2D.Length) array2D                    


let flatten2 (array2D:'a[,]) : 'a [] =
    checkNonNull "2D array" array2D
    let arr = Array.zeroCreate array2D.Length
    let x_len,y_len = array2D.GetLength(0),array2D.GetLength(1)
    
    let rec recurse x y acc (array2D:'a[,]) =
        if   x = x_len then arr
        elif y = y_len  then recurse (x+1) 0 acc array2D
        else arr.[acc] <- array2D.[x,y]
             recurse x (y+1) (acc+1) array2D
    recurse 0 0 0 array2D

let flatten3  (array2D:'a[,]) : 'a [] =     
    checkNonNull "2D array" array2D
    let xn,yn = array2D.GetLength(0)-1, array2D.GetLength(1)-1 
    let arr = Array.zeroCreate array2D.Length
    let cnt = ref 0
    for x in 0..xn do
        for y in 0..yn do
            arr.[!cnt] <- array2D.[x,y]            
            cnt := !cnt + 1
    arr

let flatten4  (array2D:'a[,]) : 'a [] =     
    checkNonNull "2D array" array2D
    let xn,yn = array2D.GetLength(0)-1, array2D.GetLength(1)-1 
    let arr = Array.zeroCreate array2D.Length
    let mutable cnt = 0
    for x in 0..xn do
        for y in 0..yn do
            arr.[cnt] <- array2D.[x,y]            
            cnt <- cnt + 1
    arr

let testArr = Array2D.create 100 100 (22)

let f1_result = Benchmark.Run (repeat 1000  (fun () -> flatten1 testArr |> ignore ))
let f2_result = Benchmark.Run (repeat 1000  (fun () -> flatten2 testArr |> ignore ))
let f3_result = Benchmark.Run (repeat 1000  (fun () -> flatten3 testArr |> ignore ))
let f4_result = Benchmark.Run (repeat 1000  (fun () -> flatten4 testArr |> ignore ))



