module LakeFunctions.PerfCalc

open MathNet.Numerics
open Cuisl

///flux,截面上的浆液通量,(m3/hr)/m2
///vel,吸收塔内的烟气流速,m/s
///dP,喷淋层的压力降,Pa
let dPSpray(flux, vel) =
    let a = 0.5276884123
    let b = 0.966426065247622
    let c = 2.69594679585481
    let d = 1.11545236039741
    a * flux ** b * exp(c * cubic(vel / 10.0)) * vel ** d

/// <summary>
/// 托盘压力降,Pa
/// </summary>
/// <param name="lg">L/G,l/m3</param>
/// <param name="vel">托盘孔内流速, m/s</param>
let trayPressureDrop =
    let rows = 
        Lake.balance.trayPressureDrop.DataRecords
        |> Array.map(fun c -> c.v, c.a, c.b, c.c, c.d, c.e)
        |> Array.sortBy(fun(v,_,_,_,_,_)->v)
    
    //use db = new LakeContext()
    //let rows =
    //    query {
    //        for c in db.trayPressureDrop do
    //            select (c.V, c.A, c.B, c.C, c.D, c.E)
    //            }
    //    |> List.ofSeq
    //    |> List.sortBy(fun(v,_,_,_,_,_)->v)

    fun lg ->
        let xs, ys =
            rows
            |> Array.map(fun(v, a, b, c, d, e) ->
                let res = a + b * lg + c * lg ** 2.0 + d * lg ** 3.0 + e * lg ** 4.0
                v, res)
            |> Array.unzip
        fun vel -> Interpolate.Linear(xs, ys).Interpolate vel

/// PerfCalc!E200.
/// totalFlux,截面上的浆液通量,(m3/hr)/m2.
/// undertrayPumpPercent, 1~100 %.
/// 返回标准密度下，标准流速下的吸收塔入口压力降，Pa
let inletDP totalFlux undertrayPumpPercent =
    //    let a = 249.082 * 0.35822662
    //    let b = 249.082 * 0.0089927552 * 0.4090397558
    //    let c = 249.082 * 0.000051434922 * 0.4090397558 ** 2.0
    //    let d = 249.082 * -0.0098574363
    //    let e = 249.082 * 0.0003304303
    //    let f = 249.082 * -0.0000034469031
    //    let g = -0.0032715736 * 0.4090397558
    //    let h = -0.017716091
    //    let i = 0.00051727781
    //    let j = -0.0000056439249
    let a = 89.22780296284
    let b = 0.916221831693378
    let c = 0.00214353940035941
    let d = -2.4553099484766
    let e = 0.0823042399846
    let f = -0.0008585615179542
    let g = -0.00133820366642573
    let h = -0.017716091
    let i = 0.00051727781
    let j = -0.0000056439249
    let numerator =
        a + b * totalFlux + c * totalFlux ** 2.0 + d * undertrayPumpPercent + e * undertrayPumpPercent ** 2.0
        + f * undertrayPumpPercent ** 3.0
    let denominator =
        1.0 + g * totalFlux + h * undertrayPumpPercent + i * undertrayPumpPercent ** 2.0
        + j * undertrayPumpPercent ** 3.0
    numerator / denominator

///托盘下没有喷淋层
///flux,截面上的浆液通量,(m3/hr)/m2
///返回标准密度下，标准流速下的吸收塔入口压力降，Pa
let dPInlet (flux) =
    let flux = flux / 100.0
    let a = 89.22780296284
    let b = 91.6221831644626 * flux
    let c = 21.4353940013129 * square flux
    let g = -0.133820366635452 * flux
    (a + b + c) / (1.0 + g)

/// PerfCalc!E201.
///flux,截面上的浆液通量,(m3/hr)/m2
///返回标准密度下，标准流速下的吸收塔入口压力降，Pa
let inletDP0Underspray(flux) =
    let a = 89.22780296284
    let b = 0.916221831693378
    let c = 0.00214353940035941
    let g = -0.00133820366642573
    (a + b * flux + c * flux ** 2.0) / (1.0 + g * flux)

///RT Solids residence time ,hours
///sr Ca/S,1.03
let pH(sr, rt) =
    let log_rt = log(rt)
    let c = 7.428751063
    let d = 0.276118578
    let e = -0.0236785
    let f = -13.3030378
    let g = 8.948521487
    let h = -3.83626805
    let i = 0.01153343
    let j = 0.016425324
    let k = -0.00304985
    let l = -1.12987632
    let nu = c + d * log_rt + e * square(log_rt) + f * sr + g * square(sr) + h * cubic(sr)
    let de = 1.0 + i * log_rt + j * square(log_rt) + k * cubic(log_rt) + l * sr
    nu / de