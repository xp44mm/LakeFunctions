namespace LakeFunctions
open Cuisl

//空气
module Air =
    ///N2的体积百分数,%
    let N2 = 79.0

    ///O2的体积百分数,%
    let O2 = 100.0 - N2

    ///N2的质量百分数,%
    let Wt_N2 = 
        let n = MaterialProperties.molar "N2" * N2
        let o = MaterialProperties.molar "O2" * O2
        n/(n+o) *100.0

    ///O2的质量百分数,%
    let Wt_O2 = 100.0 - Wt_N2

    ///空气动力粘度, 
    ///dynamic viscosity: DV, uPa*s
    ///d,相对湿度，%，查制粉系统规程，288页。
    ///需要对照传热学表格重新核对
    let dynamicViscosity t d =
        //干空气动力粘度, uPa*s
        let eff t = 
            let a = 17.14237
            let b = 0.0463604
            let c = -0.00002745836
            let e = 0.00000001811235
            let f = 67449700000.0
            let g = 0.0000000000000010227747
            a + b * t + c * t ** 2.0 + e * t ** 3.0 + f * t ** 4.0 + g * t ** 5.0

            
        let x t = 
            let j = -9.0108949
            let h = 0.02654355
            let i = -0.00006432423
            let k = 0.000001307935
            let m = -0.00000000008190284
            j + h * t + i * square t + k * cubic t + m * quartic t
        
        let d = d * 0.001
        
        eff t + d / (1.0 + d) * x t