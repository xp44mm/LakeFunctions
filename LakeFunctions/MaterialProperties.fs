module LakeFunctions.MaterialProperties

///输入分子式，
///返回摩尔质量, kg/kmol, g/mol
let molar =
    //use db = new LakeEf.LakeContext()
    let materials =
        Lake.Chemical.摩尔质量.DataRecords
        |> Array.map(fun c -> c.分子式,c.Value)
        |> Map.ofArray

        //query {
        //    for c in db.摩尔质量 do
        //        select (c.分子式,c.摩尔质量1)
        //        }
        //|> Map.ofSeq

    fun name -> materials.[name]


///输入分子式，或名称（Q235-A）
///返回比重,ton/m3
let specificGravity =
    //use db = new LakeContext()
    let materials =
        Lake.Chemical.比重.DataRecords
        |> Array.map(fun c -> c.分子式,c.Value)
        |> Map.ofArray

        //query {
        //    for c in db.比重 do
        //        select (c.分子式,c.比重1)
        //        }
        //|> Map.ofSeq

    fun name -> materials.[name]

///solubility, 溶解度, g/mL；
///formula, 分子式；
let solubility =
    //use db = new LakeContext()
    let materials =
        Lake.Chemical.溶解度.DataRecords
        |> Array.map(fun c -> c.分子式,c.Value)
        |> Map.ofArray

        //query {
        //    for c in db.溶解度 do
        //        select (c.分子式,c.溶解度1)
        //        }
        //|> Map.ofSeq

    fun name -> materials.[name]

