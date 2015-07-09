namespace FSharp.Core.ArrayFunctions
open NUnit
open NUnit.Framework
open PerfUtil
open PerfUtil.Extensions


module PerformanceTests =

    let inline checkNonNull argName arg = 
        match box arg with 
        | null -> nullArg argName 
        | _ -> ()
    

    let private loopAccInto (func:int->int->'acc*'col->'a[,]->'acc*'col) 
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
        let rec recurse x y acc =
            if   x = array2D.GetLength(0) then arr
            elif y = array2D.GetLength(1) then recurse (x+1) 0 acc 
            else arr.[acc] <- array2D.[x,y]
                 recurse x (y+1) (acc+1)
        recurse 0 0 0

