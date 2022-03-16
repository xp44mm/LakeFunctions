module LakeFunctions.Steel

open MathNet.Numerics

let private elasticAll =
    //use db = new LakeContext()
    //query {
    //    for c in db.钢材弹性模量 do
    //    select c
    //    }
    //|> Seq.groupBy(fun c -> c.材料)
    //|> Map.ofSeq
    Lake.Staad.钢材弹性模量.DataRecords
    |> Array.groupBy(fun c -> c.材料)
    |> Map.ofArray


///弹性模量,GPa
///温度,℃
let elasticModulus material =
    //let rows = Map.find material elasticAll
    //let xs,ys =
    //    rows
    //    |> Seq.map(fun row->row.温度,row.弹性模量)
    //    |> Seq.sortBy fst
    //    |> Array.ofSeq
    //    |> Array.unzip
    let xs,ys = 
        Map.find material elasticAll
        |> fun rows -> 
            rows 
            |> Array.map(fun row->row.温度,row.弹性模量)
            |> Array.sortBy fst
            |> Array.unzip
    Interpolate.Linear(xs, ys).Interpolate

let private allowableStressAll =
    //use db = new LakeContext()
    //query {
    //    for c in db.钢材许用应力 do
    //    select c
    //    }
    //|> Seq.groupBy(fun c -> c.材料)
    //|> Map.ofSeq
    Lake.Staad.钢材许用应力.DataRecords
    |> Array.groupBy(fun c -> c.材料)
    |> Map.ofArray

///许用应力,MPa
///温度,℃
let allowableStress material =
    //let rows = Map.find material allowableStressAll
    //let xs,ys =
    //    rows
    //    |> Seq.map(fun row->row.温度,row.许用应力)
    //    |> Seq.sortBy fst
    //    |> Array.ofSeq
    //    |> Array.unzip
    //let xs,ys = Map.find material allowableStressAll
    let xs,ys = 
        Map.find material allowableStressAll
        |> fun rows -> 
            rows 
            |> Array.map(fun row->row.温度,row.许用应力)
            |> Array.sortBy fst
            |> Array.unzip

    Interpolate.Linear(xs, ys).Interpolate
