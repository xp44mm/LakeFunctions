//用于替代module balance.Functions.Absorber
module LakeFunctions.AbsorberCalc
open Cuisl

//吸收塔标准烟气流速, m/s
let stdVel = 3.048

type absorberPrecalcResult = 
    {
        gasFlow:float
        diameter:float 
        area: float
        vel: float
    }

///balance.Functions.Tests.AbsorberCalcTests.absorberPrecalc
let absorberPrecalc gasFlow diameter = 
    let area = System.Math.PI / 4.0 * diameter ** 2.0
    let vel = gasFlow / 3600.0 / area
    {
        gasFlow = gasFlow
        diameter = diameter
        area = area
        vel = vel
    }

//喷淋层计算
let dPSpray = PerfCalc.dPSpray

//喷淋层类型的名称，以及运行喷淋层的数量
let sprayTypes = 
    dict ["Std on", 1
          "Std off", 0
          "IS on-on", 2
          "IS on-off", 1
          "IS off-off", 0
          "None", 0]

type SprayResult = 
    {headers: int
     liquidFlow: float
     flux: float
     dp: float
     dpstd: float
     lg: float
     lgstd: float}

let sprayCalc absorber pumpFlow sprayType aboveFlow = 
    let gasFlow = absorber.gasFlow
    let diameter= absorber.diameter
    let area    = absorber.area
    let vel     = absorber.vel
    
    //本层水泵的数量, #
    let headers = sprayTypes.[sprayType]
    let liquidFlow = pumpFlow * float headers + aboveFlow
    let lg = liquidFlow / gasFlow * 1e3
    let lgstd = lg * vel / stdVel
    //本层的通量, (m3/hr)/m2
    let flux = liquidFlow / area
    //本层压力降, Pa
    let dp = dPSpray(flux, vel)
    //本层压力降（@标准流速下）, Pa
    let dpstd = dPSpray(flux, stdVel)
    {headers = headers
     liquidFlow = liquidFlow
     flux = flux
     dp = dp
     dpstd = dpstd
     lg = lg
     lgstd = lgstd}

//托盘计算
let trayMaxLgstd = 17.8240729466667 //托盘最大液气比, l/m3
let trayPressureDrop = PerfCalc.trayPressureDrop
let trayMaxVelocity = Performance.trayMaxVelocity

//tray ng max value Standards!B29
let gohara = 1.069444377 //8 * 0.1336805471 units("gpm/1000 acfm","l/m3")

type trayResult = 
    {
        trayMaxVel: float
        trayVel: float
        trayStdVel: float
        trayDp: float
        trayStdDp: float
        trayNg:float
     }

let trayCalc absorber openArea (above: SprayResult) = 
    //let gasFlow = absorber.gasFlow
    //let diameter= absorber.diameter
    //let area    = absorber.area
    let vel     = absorber.vel

    let openAreaFlux = above.flux / openArea * 100.0
    //托盘最大开孔流速, m/s
    let trayMaxVel = trayMaxVelocity openAreaFlux
    //托盘开孔流速, m/s
    let trayVel = vel / openArea * 100.0
    //吸收塔标准流速下,托盘开孔流速, m/s
    let trayStdVel = stdVel / openArea * 100.0
    let trayDp = trayPressureDrop above.lg trayVel
    let trayStdDp = trayPressureDrop above.lgstd trayStdVel

    //PerfCalc!B63
    let ng = above.lgstd * 0.06

    {trayMaxVel = trayMaxVel
     trayVel = trayVel
     trayStdVel = trayStdVel
     trayDp = trayDp
     trayStdDp = trayStdDp
     trayNg = ng
     }

// 吸收塔入口
let inletDP = PerfCalc.inletDP
let inletDP0Underspray = PerfCalc.inletDP0Underspray

type absInletResult = 
    {inletDP0Underspray: float
     tcorrect: float
     vcorrect: float}

let absInlet totalFlux inletVelocity inletTemperature saturTemperature = 
    let tcorrect = (saturTemperature + 273.15) / (inletTemperature + 273.15)
    let vcorrect = (inletVelocity / 15.24) ** 2.0
    let inletDP0Underspray = (inletDP0Underspray totalFlux) * tcorrect * vcorrect
    {inletDP0Underspray = inletDP0Underspray
     tcorrect = tcorrect
     vcorrect = vcorrect}


type absorberInput =
    {
        gasFlow  : float
        diameter : float
        pumpFlow : float
        sprays: string []
        openArea         : float
        inletVelocity    : float
        saturTemperature : float
        inletTemperature : float
    }
// 
type absorberPressureDropResult = 
    {
        operatingHeaders: int
        totalFlow: float
        flux: float
        lg: float
        stdLg: float
        spraysDp: float
        spraysStdDp: float
        trayMaxVel: float
        trayPressureDrop: float
        trayStdPressureDrop: float
        trayNg:float
        inletPressureDrop: float
        stdDp: float

    }

let absorberPressureDrop (absorberInput:absorberInput) =
    let gasFlow          = absorberInput.gasFlow 
    let diameter         = absorberInput.diameter 
    let pumpFlow         = absorberInput.pumpFlow
    let sprays           = absorberInput.sprays
    let openArea         = absorberInput.openArea 
    let inletVelocity    = absorberInput.inletVelocity 
    let saturTemperature = absorberInput.saturTemperature 
    let inletTemperature = absorberInput.inletTemperature

    let absorber =  absorberPrecalc gasFlow diameter
    
    let sprayel6 = sprayCalc absorber pumpFlow sprays.[5] 0.0
    let sprayel5 = sprayCalc absorber pumpFlow sprays.[4] sprayel6.liquidFlow
    let sprayel4 = sprayCalc absorber pumpFlow sprays.[3] sprayel5.liquidFlow
    let sprayel3 = sprayCalc absorber pumpFlow sprays.[2] sprayel4.liquidFlow
    let sprayel2 = sprayCalc absorber pumpFlow sprays.[1] sprayel3.liquidFlow
    let sprayel1 = sprayCalc absorber pumpFlow sprays.[0] sprayel2.liquidFlow

    let sprayResults = [|sprayel1; sprayel2; sprayel3; sprayel4; sprayel5; sprayel6|]
    let hasTray = openArea > 0.0
    let tray = 
        if hasTray then
            trayCalc absorber openArea sprayel1
        else
            {
                trayMaxVel = 0.0
                trayVel = 0.0
                trayStdVel = 0.0
                trayDp = 0.0
                trayStdDp = 0.0
                trayNg = 0.0
            }

    let inlet = absInlet sprayel1.flux inletVelocity inletTemperature saturTemperature
    let spraysStdDp = sprayResults |> Seq.sumBy(fun s -> s.dpstd)
    let operatingHeaders = sprayResults |> Seq.sumBy(fun s -> s.headers)
    let stdLg = sprayel1.lgstd
    let stdDp = spraysStdDp + tray.trayStdDp

    {
        operatingHeaders = operatingHeaders
        flux = sprayel1.flux
        lg = sprayel1.lg
        stdLg = stdLg
        spraysDp = sprayResults |> Seq.sumBy(fun s -> s.dp)
        spraysStdDp = spraysStdDp
        trayMaxVel = tray.trayMaxVel
        trayPressureDrop = tray.trayDp
        trayStdPressureDrop = tray.trayStdDp
        trayNg = tray.trayNg
        stdDp = stdDp
        inletPressureDrop = inlet.inletDP0Underspray
        totalFlow = sprayel1.liquidFlow
     
     }

