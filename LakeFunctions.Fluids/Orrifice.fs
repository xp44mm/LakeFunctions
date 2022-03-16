module LakeFunctions.Fluid.Orrifice
open MathNet.Numerics

let tao =
    //use db = new LakeContext()
    //    query {
    //        for c in db.孔板Tao值 do
    //        select (c.厚度,c.Tao) //x,y
    //    }
    //    |> Array.ofSeq
    //    |> Array.sortBy fst
    //    |> Array.unzip

    let xs,ys = 
        Lake.Fluid.孔板Tao值.DataRecords
        |> Array.map(fun c -> c.厚度,c.tao)
        |> Array.sortBy fst
        |> Array.unzip


    Interpolate.Linear(xs, ys).Interpolate

//孔板最小壁厚，单位与输入参数单位相同
//管道内径
let minThick dw = 0.692 * dw ** 0.474

///孔板阻力系数，输入参数都是长度单位，单位相同即可。
///d1,孔板上游管径；
///d2,孔板下游管径；
///d0,孔板内径；
///thick,孔板厚度；
let zeta dw1 dw2 dn thick =
    let a = 1.0 - pown (dn/dw1) 2
    let c = 1.0 - pown (dn/dw2) 2
    let tao = tao (thick/dn)

    0.5*a+tao*sqrt(a*c) + pown c 2

///温度引起的背压降低，kPa
///t 介质温度,℃
let backpressureDrop t =
    0.04857 * exp(sqrt((log((t + 273.15) / 218.15)) / 0.002536)) / 1000.

//许用最大孔板阻力系数，避免气蚀
//p0 大气压,kPa
//p1 入口压力（表压）,kPa
///pt 温度压降,kPa
//dp 孔板动压头, kPa
let maxZeta p0 pt p1 dp = (p0 - pt + p1) / dp - 2.0

//孔板前直管段长度, mm
let span dw dn = 5.4 * (dw - dn)

//type Orrifice( ap       // = 101.325//大气压,kPa
//                , t        // = 50.  //介质温度,℃
//                , pin      // = 100. //入口压力（表压）,kPa
//                , flow     // = 20.  //介质流量,m3/hr
//                , dens     // = 1000. //介质密度,kg/m3
//                , pipeDia  // = 100. //管道内径,mm
//                , oriDia   // = 50. //孔板内径,mm
//    ) =
//    //如果没有达到期望的出口压力，应在下游继续增加孔板

//    let wall = orrificeThick pipeDia

//    let zeta = zeta oriDia pipeDia

//    ////孔板阻力系数
//    //    let alpha = 1. - square (oriDia / pipeDia)
//    //    //假设孔板最薄
//    //    1.8 * alpha + square alpha

//    let dp = dp flow oriDia dens
//    ////孔板动压头, kPa
//    //    let oriVelo = (flow / 3600.) / (System.Math.PI / 4. * square (oriDia * 1e-3)) //孔内流速, m/s
//    //    0.5 * (dens/1000.) * square oriVelo

//    let qishi =
//        let maxZeta = maxZeta t pin ap dp
//        maxZeta + 2.0 - zeta

////气蚀系数,气蚀系数大于2, OK!
//        //let pt = 0.04857 * exp(sqrt((log((t + 273.15) / 218.15)) / 0.002536)) / 1000. //温度因素,kPa
//        //(pin + ap - pt) / dp - zeta

//    let span = 5.4 * (pipeDia - oriDia) //孔板前直管段长度, mm

//    let pout = pin - zeta * dp //孔板出口压力(表压), kPa

//    ///阻力系数
//    member __.Zeta = zeta

//    ///出口压力,kPa
//    member __.OutletPressure = pout

//    ///孔板最小壁厚,mm
//    member __.Wall = wall

//    ///孔板前直管段长度, mm
//    member __.Span = span

//    ///气蚀系数,气蚀系数大于2, OK!
//    member __.Qishi = qishi
