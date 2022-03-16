module LakeFunctions.Combustion

///100kg煤，理论纯氧燃烧，生成的产物量,kmol
let idealCombus(coal: Coal) = 
    //kmol
    let coal = coal / Coal.molar
    
    //理论耗氧量,kmol
    let ideaO2 = coal.C + coal.H / 4.0 - coal.O / 2.0 + coal.S
    
    //生成的产物,kmol
    let ideaGas = 
        {
            Gas.zero with
                CO2 = coal.C
                O2  = - ideaO2
                H2O = coal.H / 2.0 + coal.H2O
                N2  = coal.N / 2.0
                SO2 = coal.S
        }
    ideaGas



/////
//type Coal() = 
//    member val C = 0.0 with get, set
//    member val H = 0.0 with get, set
//    member val O = 0.0 with get, set
//    member val N = 0.0 with get, set
//    member val S = 0.0 with get, set
//    member val H2O = 0.0 with get, set
//    member val A = 0.0 with get, set

/////
//type Gas() = 
//    member val H2O = 0.0 with get, set
//    member val O2 = 0.0 with get, set
//    member val N2 = 0.0 with get, set
//    member val CO2 = 0.0 with get, set
//    member val SO2 = 0.0 with get, set

/////理论纯氧燃烧，coal:kg
//let idealCombus(coal: Coal) = 
//    let coal = 
//        let kmol = Coal()
//        kmol.C <- coal.C / MaterialProperties.molar "C"
//        kmol.H <- coal.H / MaterialProperties.molar "H"
//        kmol.O <- coal.O / MaterialProperties.molar "O"
//        kmol.N <- coal.N / MaterialProperties.molar "N"
//        kmol.S <- coal.S / MaterialProperties.molar "S"
//        kmol.H2O <- coal.H2O / MaterialProperties.molar "H2O"
//        kmol
    
//    //生成的产物,kmol
//    let idealGas = Gas()
//    idealGas.CO2 <- coal.C
//    idealGas.H2O <- Seq.sum [ coal.H / 2.0
//                              coal.H2O ]
//    idealGas.O2 <- Seq.sum [ coal.C
//                             coal.H / 4.0 - coal.O / 2.0
//                             coal.S ]
//    idealGas.N2 <- coal.N / 2.0
//    idealGas.SO2 <- coal.S
//    idealGas

/////根据o2量确定空气量
//let getair (air: Gas) o2 =
//    let ratio = o2 / air.O2
//    let consump = Gas()
//    consump.H2O <- ratio * air.H2O
//    consump.O2 <- ratio * air.O2
//    consump.N2 <- ratio * air.N2
//    consump

//type CombusProduce = 
//    { air: Gas
//      flue: Gas }

/////实际煤燃烧：100kg煤在指定空气过量系数下的烟气量
/////coal,100kg
/////air,体积比
/////excess,空气过量系数
//let unitcombus (idealGas: Gas) (air: Gas) (excess: float) = 
//    //消耗的空气量,kmol
//    let air = 
//        let ratio = excess * idealGas.O2 / air.O2
//        let consump = Gas()
//        consump.H2O <- ratio * air.H2O
//        consump.O2 <- ratio * air.O2
//        consump.N2 <- ratio * air.N2
//        consump
    
//    //总烟气,kmol
//    let flue = Gas()
//    flue.H2O <- idealGas.H2O + air.H2O
//    flue.O2 <- idealGas.O2 * (excess - 1.0)
//    flue.N2 <- idealGas.N2 + air.N2
//    flue.CO2 <- idealGas.CO2
//    flue.SO2 <- idealGas.SO2
//    { air = air
//      flue = flue }

//let total(gas: Gas) = Seq.sum [ gas.H2O; gas.O2; gas.N2; gas.CO2; gas.SO2 ]

//let percent(gas: Gas) = 
//    let total = total gas
//    let p = Gas()
//    p.H2O <- gas.H2O / total * 100.0
//    p.O2 <- gas.O2 / total * 100.0
//    p.N2 <- gas.N2 / total * 100.0
//    p.CO2 <- gas.CO2 / total * 100.0
//    p.SO2 <- gas.SO2 / total * 100.0
//    p
    
/////已知烟气中氧气含量，获取过量空气系数
//let getexcess (idealGas: Gas) (air: Gas) (o2: float) = 
//    //两分法求过量空气系数
//    let rec bisect emin emax = 
//        if emax - emin < 1e-5 then (emin + emax) / 2.0
//        else 
//            let excess = (emin + emax) / 2.0
//            let result = unitcombus idealGas air excess
//            let p = percent result.flue
//            if p.O2 > o2 then bisect emin excess
//            else bisect excess emax
    
//    //求过量空气系数的上边界
//    let rec crossover excess = 
//        let result = unitcombus idealGas air excess
//        let p = percent result.flue
//        if p.O2 > o2 then excess
//        else crossover(excess + 1.0)
    
//    //求过量空气系数，注意过量空气系数的下边界
//    if o2 = 0.0 then 1.0
//    else bisect 1.0 (crossover 2.0)

/////烟气量
//let gasflow (idealGas: Gas) (air: Gas) (excess: float) (wtCoal: float) = 
//    let result = unitcombus idealGas air excess
//    let total = total result.flue
//    wtCoal / 100.0 * total * 22.414

/////根据烟气量求过量空气系数
//let getexcessByFlow (idealGas: Gas) (air: Gas) (wtCoal: float) (flow: float) = 

//    //两分法求过量空气系数
//    let rec bisect emin emax = 
//        if emax - emin < 1e-5 then (emin + emax) / 2.0
//        else 
//            let excess = (emin + emax) / 2.0
//            let result = unitcombus idealGas air excess
//            let total = total result.flue
//            let flow' = wtCoal / 100.0 * total * 22.414

//            if flow' > flow then bisect emin excess
//            else bisect excess emax
    
//    //求过量空气系数的上边界
//    let rec crossover excess = 
//        let result = unitcombus idealGas air excess
//        let total = total result.flue
//        let flow' = wtCoal / 100.0 * total * 22.414

//        if flow' > flow then excess
//        else crossover(excess + 1.0)
    
//    let minflow = gasflow idealGas air 1.0 wtCoal

//    //求过量空气系数，注意过量空气系数的下边界
//    if minflow = flow then 1.0
//    else bisect 1.0 (crossover 2.0)
