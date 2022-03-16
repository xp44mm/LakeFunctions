module LakeFunctions.GasBalance

type absorberOutletResult =
    {
        absOutlet: GasFlow
        absOutletVolume: float
        ppmSO2:float //吸收塔入口
    }

let absorberOutlet (terminal: GasFlow) (oxiair: GasFlow) (removal: Gas) =
    let reactHeat = Limestone.limestoneReactHeat removal
    let evaporativeCooling = Energy.evaporativeCooling reactHeat
    let absOutlet = GasFlow.create((terminal.Ingredient()) + (oxiair.Ingredient()) - removal, p = terminal.pressure)
    let temp, evapor = evaporativeCooling {absOutlet with temperature = terminal.temperature}
    let absOutlet =
        { absOutlet with
            H2O = absOutlet.H2O + evapor
            temperature = temp
        }
    let absOutletVolume = GasFlowPerf.totalVolume absOutlet

    let ppmSO2 =
        let absInlet =
            {(terminal.Ingredient()) with 
                H2O = 0.0
                ash = 0.0}
            / Gas.molar
        absInlet.SO2 / absInlet.total() * 1e6

    let rtuSO2 = Performance.rtu_so2 ppmSO2

    {
        absOutlet = absOutlet
        absOutletVolume = absOutletVolume

        ppmSO2 = ppmSO2
        //rtuSO2 = rtuSO2

    }

//open GGH

type gghData() =
    member val dirtyPressureDrop = 0.0 with get, set
    member val cleanPressureDrop = 0.0 with get, set
    member val dirtyLeakage = 0.0 with get, set
    member val cleanLeakage = 0.0 with get, set
    member val temperature = 0.0 with get, set

type gghOutletResult =
    {
        gghEnth: float
        absInlet: GasFlow
        absOutlet: GasFlow
        gghOutlet: GasFlow

        absOutletVolume: float
        ppmSO2:float
        //rtuSO2:float
    }

let gghOutlet (terminal: GasFlow) (oxiair: GasFlow) (removal: Gas)(effect:Gas) (ggh: gghData)(sootblow:GasFlow) =

    let leakage = new GghLeakage(effect, ggh.dirtyLeakage, ggh.cleanLeakage)
    let towerOverall = leakage.towerOverall
    let absInlet, absOutlet, gghOutlet = leakage.mass(terminal.Ingredient(), removal, oxiair.Ingredient(), sootblow.Ingredient())

    let heatSootblow = GasFlowPerf.enthalpy sootblow
    let heatGghInlet = GasFlowPerf.enthalpy terminal
    //prepare
    let reactHeat = Limestone.limestoneReactHeat removal
    let evaporativeCooling = Energy.evaporativeCooling reactHeat
    let reaction = GasFlow.create(absOutlet, p = terminal.pressure + ggh.cleanPressureDrop)

    let rec loop tmin tmax =
        let temp = 0.5 * (tmin + tmax)

        let rec loopE evapor =
            let reaction =
                {reaction with H2O = reaction.H2O + towerOverall * evapor
                               temperature = temp}

            let saturTemp, evaporRec = evaporativeCooling reaction
            if abs(evaporRec - evapor) < 1e-4 then saturTemp, evaporRec
            else loopE evaporRec

        let saturTemp, evapor = loopE 0.0

        let absInlet =
            let ingr = {absInlet with H2O = absInlet.H2O + evapor * towerOverall}
            GasFlow.create(ingr, t = temp)

        let absOutlet =
            let ingr = {absOutlet with H2O = absOutlet.H2O + evapor * (1.0 + towerOverall)}
            GasFlow.create(ingr, t = saturTemp, p = reaction.pressure)

        let gghOutlet =
            let ingr =
                {gghOutlet with H2O = gghOutlet.H2O + evapor * (1.0 + towerOverall) * (1.0 - ggh.cleanLeakage / 100.0)}
            GasFlow.create(ingr, t = ggh.temperature, p = terminal.pressure)

        let heatAbsInlet = GasFlowPerf.enthalpy absInlet
        let heatAbsOutlet = GasFlowPerf.enthalpy absOutlet
        let heatGghOutlet = GasFlowPerf.enthalpy gghOutlet

        let hen =
            (heatSootblow + heatGghInlet) * (1.0 - ggh.dirtyLeakage / 100.0) + heatAbsOutlet * ggh.cleanLeakage / 100.0
        let hex =
            (heatSootblow + heatGghInlet) * ggh.dirtyLeakage / 100.0 + heatAbsOutlet * (1.0 - ggh.cleanLeakage / 100.0)
        //放热
        let fang = hen - heatAbsInlet
        //吸热
        let xi = heatGghOutlet - hex
        if tmax - tmin < 1e-5 then
            let ppmSO2 =
                let absInlet =
                    {absInlet.Ingredient() with 
                        H2O = 0.0
                        ash = 0.0}
                    / Gas.molar
                absInlet.SO2 / absInlet.total() * 1e6

            let rtuSO2 = Performance.rtu_so2 ppmSO2

            {gghEnth = max fang xi
             absInlet = absInlet
             absOutlet = absOutlet
             gghOutlet = gghOutlet

             absOutletVolume = GasFlowPerf.totalVolume absOutlet
             ppmSO2 = ppmSO2
             //rtuSO2 = rtuSO2

             }
        elif fang > xi then loop temp tmax
        else loop tmin temp

    loop ggh.temperature terminal.temperature