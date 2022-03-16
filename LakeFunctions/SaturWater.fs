module LakeFunctions.SaturWater

open MathNet.Numerics

///返回水在输入温度下的饱和压力，MPa;
///输入饱和温度，℃;
let saturPressure =
    //use db = new LakeContext()
        //query {
        //    for c in db.饱和水 do
        //        select (c.温度,c.压力) //x,y
        //        }
        //|> Array.ofSeq
        //|> Array.sortBy fst
        //|> Array.unzip
    let xs,ys = 
        Lake.Thermal.饱和水.DataRecords
        |> Array.map(fun c -> c.温度,c.压力)
        |> Array.sortBy fst
        |> Array.unzip

    Interpolate.Linear(xs, ys).Interpolate


///返回水在输入压力下的饱和温度，℃;
///输入饱和压力，MPa;
let saturTemprature =
    //use db = new LakeContext()
        //query {
        //    for c in db.饱和水 do
        //        select (c.压力,c.温度) //x,y
        //        }
        //|> Array.ofSeq
        //|> Array.sortBy fst
        //|> Array.unzip

    let xs,ys = 
        Lake.Thermal.饱和水.DataRecords
        |> Array.map(fun c -> c.压力,c.温度)
        |> Array.sortBy fst
        |> Array.unzip

    Interpolate.Linear(xs, ys).Interpolate


///返回汽化潜热,kJ/kg;
///输入饱和压力，MPa
let latentHeat =
    //use db = new LakeContext()
        //query {
        //    for c in db.饱和水 do
        //        select (c.压力,c.汽化潜热) //x,y
        //        }
        //|> Array.ofSeq
        //|> Array.sortBy fst
        //|> Array.unzip


    let xs,ys = 
        Lake.Thermal.饱和水.DataRecords
        |> Array.map(fun c -> c.压力,c.汽化潜热)
        |> Array.sortBy fst
        |> Array.unzip

    Interpolate.Linear(xs, ys).Interpolate


///返回液体比焓,kJ/kg
///输入饱和压力，MPa
let enthalpy =
    //use db = new LakeContext()
        //query {
        //    for c in db.饱和水 do
        //        select (c.压力,c.比焓) //x,y
        //        }
        //|> Array.ofSeq
        //|> Array.sortBy fst
        //|> Array.unzip

    let xs,ys = 
        Lake.Thermal.饱和水.DataRecords
        |> Array.map(fun c -> c.压力,c.比焓)
        |> Array.sortBy fst
        |> Array.unzip


    Interpolate.Linear(xs, ys).Interpolate
        
