module LakeFunctions.UnitConverter

open System

let degC(degF) = (degF - 32.0) / 1.8
let degF(degC) = 1.8 * degC + 32.0

///单位转换因子
///1 ft = 0.3048 m
///1 * alter("ft","m") = 0.3048 
let units  =
    //use db = new LakeContext()
        //query {
        //    for c in db.unitsFactor do
        //        select ((c.Source,c.Target),c.Factor)
        //        }
        //|> Map.ofSeq
    
    let lookup = 
        Lake.balance.unitsFactor.DataRecords
        |> Array.map(fun c -> (c.source,c.target),c.factor)
        |> Map.ofArray
    fun (source,target) -> lookup.[source,target]


let factor = units







