module LakeFunctions.LiquidPerf

let tf (liquid:Liquid) = liquid.toSeq() |> Seq.sumBy snd

///total suspended solid 总悬浮固体,kg/hr
let tss (liq: Liquid) =
    (liq * Liquid.suspend).toSeq()
    |> Seq.sumBy snd

///total desolved solid 总溶解固体,kg/hr
let tds (liq:Liquid) =
    (liq * Liquid.ion).toSeq()
    |> Seq.sumBy snd

///free water 自由水，溶液,kg/hr
let fw (liquid:Liquid) = tds liquid + liquid.H2O

///concentration 氯离子浓度,ppmwt
let concCl (liquid:Liquid) = liquid.``Cl-``/ fw liquid * 1e6

///含固量,%
let solids (liquid:Liquid) =
    let tss = tss liquid
    let total = tss + fw liquid
    tss / total * 100.0

///Specific Gravity比重，无量纲
///SG sln,溶液比重
let sgSolution (liquid:Liquid) =
    tds liquid /fw liquid + 1.0

///悬浮浆液相对于水的比重，无量纲
///SG solid
let suspendedSpecificGravity (suspend:Liquid) =
    let wt = tss suspend
    if wt > 0.0 then
        let sgwt = Liquid.suspendSG * suspend
        tss sgwt / wt
    else 1.0

///没有考虑温度修正
///浆液的密度,kg/m3
let density (liquid:Liquid) =
    let solids = solids liquid
    let sgSolution = sgSolution liquid
    let sgSolid = suspendedSpecificGravity liquid
    let sg = 100./((solids/sgSolid)+((100.-solids)/sgSolution))
    sg * 1e3