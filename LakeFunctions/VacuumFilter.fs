module LakeFunctions.VacuumFilter
open Cuisl

type result =
    {
        wash:float
        gypsum:Liquid
        filtrate:Liquid
    }

//冲洗水质量
let wash gyp concCl =
    let fw = LiquidPerf.fw gyp
    let cl = gyp.``Cl-`` / fw * 1e6
    let solids = LiquidPerf.solids gyp / 1e2
    let gFw = fw / (LiquidPerf.sgSolution gyp * 1e3)
    let ionScale = cl / concCl
    let dispeff = 70e-2
    gFw * log(ionScale / (1.0 - solids)) / log(1.0 - dispeff) * 1e3

///CakeCl:Cl-倍率：冲洗后Cl-浓度/冲洗前Cl-浓度
///cl离子浓度,ppm=cl离子质量/自由水质量*1e6,如12000,20000ppm
///moisture：石膏质量含湿量,%=水量/TF(总流量)*100
///Displacement Eff,%：替换效率，典型值70
///Displacements:冲洗水替代倍率，是指滤饼冲洗水与石膏自由水体积的最小比值，典型值1.7
let CakeCl(moisture, displacementEff, displacements) =
    moisture / 100.0 * (1.0 - displacementEff/100.0)**displacements

/////计算出浆液的fw，返回浆液
//let fwLiquid (productA: Liquid) susp solids concCl =
//    let tss = LiquidPerf.tss susp
//    let fw = tss / solids * (1.0 - solids)
//    let cl = concCl / 1e6 * fw
//    let scale = cl / productA.``Cl-``
//    let ion = Liquid.unit(scale) * productA
//    let tds = LiquidPerf.tds ion
//    {(Liquid.suspend * susp + Liquid.ion * ion) with H2O = fw - tds}
//
//
//let balance (productA: Liquid) (y:Liquid) solids concCl (feed:Liquid) =
//    let gypsum =
//        let susp = feed * y
//        fwLiquid productA susp solids concCl
//
//    let wash = wash gypsum concCl
//    let filtrate =
//        let f = feed - gypsum
//        {f with H2O = f.H2O + wash}
//
//    {
//        wash = wash
//        gypsum = gypsum
//        filtrate = filtrate
//    }