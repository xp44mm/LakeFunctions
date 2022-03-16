module LakeFunctions.Removal

//open GGH

//有ggh
let removal (inlet:Gas) (effect:Gas) dirtyLeakage cleanLeakage =
    let ggh = new GghLeakage(effect, dirtyLeakage, cleanLeakage)
    inlet * ggh.overallEffect

//无ggh
let simpleRemoval (inlet:Gas) (effect:Gas) =
    let res = inlet * effect * Gas.unit 0.01
    { Gas.zero with
        SO2 = res.SO2
        SO3 = res.SO3
        HCl = res.HCl
        HF  = res.HF
        ash = res.ash
    }