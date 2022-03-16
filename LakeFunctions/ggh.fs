namespace LakeFunctions

type Ggh() =
    member val dirtyPressureDrop = 0.0 with get, set
    member val cleanPressureDrop = 0.0 with get, set
    member val dirtyLeakage = 0.0 with get, set
    member val cleanLeakage = 0.0 with get, set
    member val temperature = 0.0 with get, set
    member val sootblow = 0.0 with get, set

type GghLeakage(effect : Gas, dirtyLeakage,cleanLeakage) =
    member this.towerOverall = cleanLeakage / (100.0 - cleanLeakage)

    /// 折算系数：gghInlet + sootblow
    member this.gghOverall =
        effect.apply (fun eff -> (100.0 - dirtyLeakage) / (100.0 - cleanLeakage * (1.0 - eff / 100.0)))

    /// 总体效率:0~1
    member this.overallEffect = this.gghOverall * effect / Gas.unit (100.0)

    /// 不考虑蒸发水的情况时，吸收塔出入口、ggh出口的质量
    member this.mass (gghInlet, removal:Gas, oxiair, sootblow) =
        let passive =
            { Gas.zero with
                H2O  = removal.H2O
                O2   = removal.O2
                CO2  = removal.CO2
            }

        let absInlet =
            this.gghOverall * (sootblow + gghInlet) +
            Gas.unit (this.towerOverall) * (oxiair - passive)

        let absOutlet = absInlet + oxiair - removal

        let gghOutlet =
            (sootblow + gghInlet) * Gas.unit (dirtyLeakage / 100.0)
            + absOutlet * Gas.unit (1.0 - cleanLeakage / 100.0)
        absInlet, absOutlet, gghOutlet