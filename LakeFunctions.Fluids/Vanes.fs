namespace LakeFunctions.Fluid.Ducts

module Vanes = 

    ///90°弯头对角线的长度
    let diagonal b1 b2 = sqrt (b1**2.0 + b2**2.0)
    
    let vanes b1 b2 rn = 
        let s = diagonal b1 b2
        let n = round (0.65 * s / rn)
        seq { 
            let s1 = s / 2. / (n + 1.)
            let ds = 2.0 * s1 / n
            for i in 0.0..(n - 1.0) -> s1 + ds * i
        }
    
    let lessVanes b1 b2 rn = 
        let s = diagonal b1 b2
        let n = round (s / rn)
        seq { 
            let s1 = 2. * s / 3. / (n + 1.)
            let ds = s1 / n
            for i in 0.0..(n - 1.0) -> s1 + ds * i
        }
    
    let internal test() = 
        let b1 = 100.
        let b2 = 120.
        let rn = 10.
        ()