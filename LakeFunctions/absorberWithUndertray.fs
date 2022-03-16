module LakeFunctions.absorberWithUndertray

open AbsorberCalc

type AbsInletWithUndersprayResult = 
    {liquidFlow: float
     flux: float
     dp: float
     //dpstd: float
     lg: float
     lgstd: float
     inletDP0Underspray: float}

//有托盘下喷淋层的吸收塔入口
let absInletWithUnderspray (absorber: absorberPrecalcResult) undertrayPumpFlow 
    aboveFlow inletVelocity inletTemperature saturTemperature = 
    let gasFlow = absorber.gasFlow
    //let diameter = absorber.diameter
    let area = absorber.area
    let vel = absorber.vel
    let liquidFlow = undertrayPumpFlow + aboveFlow
    let percent = undertrayPumpFlow / liquidFlow * 1e2
    let flux = liquidFlow / area
    
    let tcorrect, vcorrect, inletDP0Underspray = 
        let r = absInlet flux inletVelocity inletTemperature saturTemperature
        r.tcorrect, r.vcorrect, r.inletDP0Underspray
    
    //PerfCalc!I200
    let inletDP = (inletDP flux percent) * tcorrect * vcorrect
    //PerfCalc!I202
    let underSprayDP = inletDP - inletDP0Underspray
    let lg = liquidFlow / gasFlow * 1e3
    let lgstd = lg * vel / stdVel
    {liquidFlow = liquidFlow
     flux = flux
     dp = underSprayDP
     //dpstd = underSprayDP //注意
     lg = lg
     lgstd = lgstd
     inletDP0Underspray = inletDP0Underspray}


//带下喷淋层的吸收塔计算
type AbsorberWithUndertrayInput = 
    {gasFlow: float
     diameter: float
     pumpFlow: float
     sprays: string []
     openArea: float
     undersprayFlow: float
     inletVelocity: float
     saturTemperature: float
     inletTemperature: float}

// 
type AbsorberWithUndertrayResult = 
    {operatingHeaders: int
     totalFlow: float
     flux: float
     lg: float
     stdLg: float
     spraysDp: float
     spraysStdDp: float
     trayMaxVel: float
     trayPressureDrop: float
     trayStdPressureDrop: float
     trayNg: float
     undersprayPressureDrop: float
     inletPressureDrop: float
     stdDp: float}

let absorberWithUndertray(absorberInput: AbsorberWithUndertrayInput) = 
    let gasFlow = absorberInput.gasFlow
    let diameter = absorberInput.diameter
    let pumpFlow = absorberInput.pumpFlow
    let sprays = absorberInput.sprays
    let openArea = absorberInput.openArea
    let undersprayFlow = absorberInput.undersprayFlow
    let inletVelocity = absorberInput.inletVelocity
    let saturTemperature = absorberInput.saturTemperature
    let inletTemperature = absorberInput.inletTemperature

    let absorber = absorberPrecalc gasFlow diameter
    let sprayel6 = sprayCalc absorber pumpFlow sprays.[5] 0.0
    let sprayel5 = sprayCalc absorber pumpFlow sprays.[4] sprayel6.liquidFlow
    let sprayel4 = sprayCalc absorber pumpFlow sprays.[3] sprayel5.liquidFlow
    let sprayel3 = sprayCalc absorber pumpFlow sprays.[2] sprayel4.liquidFlow
    let sprayel2 = sprayCalc absorber pumpFlow sprays.[1] sprayel3.liquidFlow
    let sprayel1 = sprayCalc absorber pumpFlow sprays.[0] sprayel2.liquidFlow
    let sprayResults = [|sprayel1; sprayel2; sprayel3; sprayel4; sprayel5; sprayel6|]
    let hasTray = openArea > 0.0
    
    let tray = 
        if hasTray then trayCalc absorber openArea sprayel1
        else 
            {trayMaxVel = 0.0
             trayVel = 0.0
             trayStdVel = 0.0
             trayDp = 0.0
             trayStdDp = 0.0
             trayNg = 0.0}
    
    //
    let hasUnderspray = hasTray && undersprayFlow > 0.0
    
    let undersprayResult = 
        if hasUnderspray then 
            absInletWithUnderspray absorber undersprayFlow sprayel1.liquidFlow inletVelocity 
                inletTemperature saturTemperature
        else 
            let inlet = absInlet sprayel1.flux inletVelocity inletTemperature saturTemperature
            {liquidFlow = sprayel1.liquidFlow
             flux = sprayel1.flux
             lg = sprayel1.lg
             lgstd = sprayel1.lgstd
             dp = 0.0
             inletDP0Underspray = inlet.inletDP0Underspray}
    
    //汇总数据
    let operatingHeaders = sprayResults |> Seq.sumBy(fun s -> s.headers)
    let spraysStdDp = sprayResults |> Seq.sumBy(fun s -> s.dpstd)
    let stdLg = undersprayResult.lgstd
    let stdDp = spraysStdDp + tray.trayStdDp + undersprayResult.dp

    {operatingHeaders = operatingHeaders
     flux = undersprayResult.flux
     lg = undersprayResult.lg
     stdLg = stdLg
     spraysDp = sprayResults |> Seq.sumBy(fun s -> s.dp)
     spraysStdDp = spraysStdDp
     trayMaxVel = tray.trayMaxVel
     trayPressureDrop = tray.trayDp
     trayStdPressureDrop = tray.trayStdDp
     trayNg = tray.trayNg
     undersprayPressureDrop = undersprayResult.dp
     inletPressureDrop = undersprayResult.inletDP0Underspray
     stdDp = stdDp
     totalFlow = undersprayResult.liquidFlow}
