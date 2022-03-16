module LakeFunctions.Equipments

let 箱子重量 =
    let ribScale = 1.4 // 加固肋系数
    let wtScale = 1.25 // 重量系数

    let boxweight dia h bp sp tp =
        let area = System.Math.PI / 4.0 * pown dia 2
        let peri = System.Math.PI * dia

        (bp * area + sp * h * peri + tp * area * ribScale) * wtScale * 7.85 / 1000.0

    //use db = new LakeContext()

    let tankWeightRows = Lake.Equipments.箱子.DataRecords
        //query{
        //    for c in db.箱子 do
        //        select c}
        //|> List.ofSeq
        //|> List.map(fun props ->
        //    let d = props.直径
        //    let b = props.底板
        //    let s = props.侧壁
        //    let t = props.顶板
        //    d,b,s,t)

    fun dia h ->
        //各壁面的板厚
        let bp, sp, tp =
            tankWeightRows
            |> Seq.filter(fun r -> r.直径 >= dia)
            |> Seq.minBy(fun r -> r.直径)
            |> fun r -> r.底板,r.侧壁,r.顶板

        boxweight dia h bp sp tp

///用于计算箱子的搅拌器功率
let 搅拌器功率 =

    //有的电机没有实践对应的箱子容积，需要注意补充。
    let ls = 
        Lake.Equipments.搅拌器.DataRecords
        |> Array.choose(fun data -> if data.容积.HasValue then Some (data.容积.Value, data.功率) else None)
    //m3
    fun volume -> 
        ls 
        |> Array.find(fun(v,pwr)-> volume <= v)
        |> snd
        //step.Interpolate(-volume)

/// <summary>
/// m3/hr->0~100.0%
/// </summary>
let 泵效率 volume =
    Lake.Equipments.泵效率.DataRecords
    |> Array.findBack(fun p -> p.流量 <= volume)
    |> fun p -> p.效率

/// <summary>
/// 电机功率：kW, flow:m3/min,实际状态下的气体流量, press: kPa
/// </summary>
let 罗茨风机功率 =

    let dataRows = Lake.Equipments.罗茨风机.DataRecords
        //query{
        //    for c in db.罗茨风机 do
        //        select c}
        //|> List.ofSeq
        //|> List.map(fun props ->
        //    let f =  props.流量
        //    let p =  props.排气压力
        //    let n =  props.电机功率
        //    f,p,n)

    //flow:m3/hr
    fun flow press ->
        dataRows
        |> Array.find(fun data -> data.流量 >= flow && data.排气压力 >= press)
        |> fun data -> data.电机功率
