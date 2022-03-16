module LakeFunctions.Limestone


open System.Collections.Generic
open System
//open LakeEf

type LimestoneAnalysis() = 
    member val availableMgCO3 = 0.0 with get, set
    member val CaCO3 = 0.0 with get, set
    member val MgCO3 = 0.0 with get, set

    member val stoich = 0.0 with get, set
    member val solids = 0.0 with get, set
    member val grind = 0.0 with get, set

//溶质；溶解物
type Solute =
    {
        ``CaSO4*2H2O`` : float
        ``CaSO3*(1/2)H2O`` : float
        ``CaCO3`` : float
        ``CaCl2`` : float
        ``CaF2`` : float
        ``MgSO4`` : float
        ``MgCO3`` : float
        ``MgCl2`` : float
        ``MgF2`` : float
        ``inerts`` : float
        ``ash`` : float
    }

type Dissolved =
    {
        MgSO4 : float
        CaCl2 : float
        MgCl2 : float
        CaF2 : float
        MgF2 : float
    }
type limestoneResult = 
    { limestone : Liquid
      removal : Gas
      productA : Liquid

      reactHeat:float
      tss:float

      rtuSr:float
      rtuGrind:float
    }

///reagent:"Lime"/"Limestone";
///components,参与反应的污染物质量,kg
let private reactHeatBy reagent (removal: Gas) = 
    let isLime = System.String.Equals(reagent, "Lime", System.StringComparison.OrdinalIgnoreCase)
    
    let removal = removal.apply(abs) / Gas.molar

    [ (*SO2 -> SO3--*) 613135.356846408, 58619.3915143368, (removal.SO2 - removal.O2 * 2.0)
      (*SO2 -> SO4--*) 454223.468344529, 340777.497902512, removal.O2 * 2.0
      (*HCl         *) 278294.37060071, 164852.619294465, removal.HCl
      (*HF          *) 152701.841720641, 90455.6514183127, removal.HF
      (*SO3         *) 766260.800986912, 73259.0958808352, removal.SO3 ]
    |> List.sumBy (fun (lime, limestone, kmol) -> 
           //cm 摩尔反应热，kJ/kmol
           let cm = 
               if isLime then lime
               else limestone
           cm * kmol)

[<Obsolete("替换为limestoneReactHeat")>]
let reactHeat = reactHeatBy "Limestone" 

///removal,参与反应的污染物质量,kg
let private reactHeatWith tbl (removal: Gas) =
    //kmol
    let removal = removal.apply(abs) / Gas.molar

    [   tbl "SO3--", (removal.SO2 - removal.O2 * 2.0)
        tbl "SO4--", removal.O2 * 2.0
        tbl "HCl", removal.HCl
        tbl "HF", removal.HF
        tbl "SO3", removal.SO3 ]
    |> List.sumBy (fun (cm, kmol) -> cm * kmol)
    
let limestoneReactHeat =  
    //use db = new LakeContext()
    //let materials =
    //    query {
    //        for c in db.石灰石反应热 do
    //            select (c.分子式,c.反应热)
    //            }
    //    |> Map.ofSeq
    let materials = 
        Lake.Thermal.石灰石反应热.DataRecords
        |> Array.map(fun c -> c.分子式,c.反应热)
        |> Map.ofArray

    //cm 摩尔反应热，kJ/kmol
    let tbl = fun name -> materials.[name]
    
    //lookup {
    //    PartitionKey = "Thermal"
    //    RowKey = "反应热/石灰石"
    //    Key = "分子式"
    //    Value = "反应热"
    //}
    reactHeatWith tbl

let limeReactHeat =  
    let materials = 
        Lake.Thermal.石灰反应热.DataRecords
        |> Array.map(fun c -> c.分子式,c.反应热)
        |> Map.ofArray


    //use db = new LakeContext()
    //let materials =
    //    query {
    //        for c in db.石灰反应热 do
    //            select (c.分子式,c.反应热)
    //            }
    //    |> Map.ofSeq

    //cm 摩尔反应热，kJ/kmol
    let tbl = fun name -> materials.[name]

    ////cm 摩尔反应热，kJ/kmol
    //let tbl = lookup {
    //    PartitionKey = "Thermal"
    //    RowKey = "反应热/石灰"
    //    Key = "分子式"
    //    Value = "反应热"
    //}
    reactHeatWith tbl

///固体生成productA
let dissolve (solute:Solute) =
    let pMgSO4 = 98.0
    let pCaF2 = 2.0
    let pMgF2 = 2.0
    let susp = 
        { Liquid.zero with
            ``CaSO4*2H2O`` = solute.``CaSO4*2H2O``
            ``CaSO3*(1/2)H2O`` = solute.``CaSO3*(1/2)H2O``
            ``CaCO3`` = solute.``CaCO3``
            ``MgSO4`` = solute.``MgSO4``*(1.0-pMgSO4/100.0)
            ``MgCO3`` = solute.``MgCO3``
            ``inerts`` = solute.``inerts``
            ``ash`` = solute.``ash``
            ``CaF2`` = solute.``CaF2``*(1.0-pCaF2/100.0)
            ``MgF2`` = solute.``MgF2``*(1.0-pMgF2/100.0)
        }

    let diss =
        {
            MgSO4 = solute.MgSO4*pMgSO4/100.0 
            CaCl2 = solute.CaCl2
            MgCl2 = solute.MgCl2
            CaF2 = solute.CaF2*pCaF2/100.0
            MgF2 = solute.MgF2*pMgF2/100.0
        }

    let diss =
        {
            MgSO4 = diss.MgSO4 / MaterialProperties.molar "MgSO4"
            CaCl2 = diss.CaCl2 / MaterialProperties.molar "CaCl2"
            MgCl2 = diss.MgCl2 / MaterialProperties.molar "MgCl2"
            CaF2 = diss.CaF2   / MaterialProperties.molar "CaF2"
            MgF2 = diss.MgF2   / MaterialProperties.molar "MgF2"
        }

    let ion =
        { Liquid.zero with
            ``Cl-`` = 2.0 * (diss.CaCl2+diss.MgCl2)
            ``F-`` = 2.0 * (diss.CaF2+diss.MgF2)
            ``Mg++`` = diss.MgCl2+diss.MgF2+diss.MgSO4
            ``Ca++`` = diss.CaCl2+diss.CaF2
            ``SO4--`` = diss.MgSO4
        }

    susp + ion * Liquid.molar

///石灰石耗量, kg
let consumption (limestone : LimestoneAnalysis) (removal : Gas) = 
    let kmolCaCO3 = limestone.CaCO3 / MaterialProperties.molar "CaCO3"
    let kmolMgCO3 = limestone.MgCO3 / MaterialProperties.molar "MgCO3"
    //CaCO3*MgCO3
    let dolomite = kmolMgCO3 * (1.0 - limestone.availableMgCO3 / 100.0)
    //kmol/100 kg
    let availAlk = kmolCaCO3 + kmolMgCO3 - 2.0 * dolomite
    
    //输入的石灰石量, kmol/hr
    let alkalinity = GasFlowPerf.alkalinity limestone.stoich removal 
    
    let scale = alkalinity / availAlk
    
    //Limestone Consumption,kg/hr
    let limestone = 
        [ "CaCO3", limestone.CaCO3
          "MgCO3", limestone.MgCO3
          "inerts", 100.0 - limestone.CaCO3 - limestone.MgCO3 
          "H2O", 100.0/limestone.solids*(100.0-limestone.solids)]
        |> Seq.map (fun (nm, kg) -> nm, kg * scale)
        |> dict
    
    limestone


///石灰石反应后的产物
let react (limestone : LimestoneAnalysis) (removal : Gas) = 
    let rtuSr = Performance.rtu_sr limestone.stoich
    let rtuGrind = Performance.rtu_grind limestone.grind

    //投入干石灰石的总质量,kg/hr
    let ls = consumption limestone removal
    let kmolTotalCaCO3 = ls.["CaCO3"] / MaterialProperties.molar "CaCO3"
    let kmolTotalMgCO3 = ls.["MgCO3"] / MaterialProperties.molar "MgCO3"

    //投入干石灰石中可用的碱,kmol/hr
    let avlMgCO3 = kmolTotalMgCO3 * limestone.availableMgCO3 / 100.0
    //Ca氧化率
    let oxirate = 99.7 //%
    
    //kg -> kmol
    let kmolRemoval = removal / Gas.molar
    
    //产物
    let producedSolids = 
        //石灰石中Ca,Mg反应的明细，kmol
        let chemicals = new Dictionary<string * string, float>(HashIdentity.Structural)
        chemicals.[("Mg", "HCl")] <- min (kmolRemoval.HCl / 2.0) (avlMgCO3)
        chemicals.[("Mg", "HF")] <- min (kmolRemoval.HF / 2.0) (avlMgCO3 - chemicals.[("Mg", "HCl")])
        chemicals.[("Mg", "SO2")] <- min (kmolRemoval.SO2) 
                                         (avlMgCO3 - chemicals.[("Mg", "HCl")] - chemicals.[("Mg", "HF")])

        chemicals.[("Ca", "HCl")] <- kmolRemoval.HCl / 2.0 - chemicals.[("Mg", "HCl")]
        chemicals.[("Ca", "HF")] <- kmolRemoval.HF / 2.0 - chemicals.[("Mg", "HF")]
        chemicals.[("Ca", "SO2")] <- kmolRemoval.SO2 - chemicals.[("Mg", "SO2")]
        chemicals.[("Ca", "SO3")] <- kmolRemoval.SO3
        //消耗Ca的总摩尔数，kmol
        let csmCaCO3 = 
            [ chemicals.[("Ca", "HCl")]
              chemicals.[("Ca", "HF")]
              chemicals.[("Ca", "SO2")]
              chemicals.[("Ca", "SO3")] ]
            |> List.sum
        Map.ofSeq [ "CaSO4*2H2O", chemicals.[("Ca", "SO2")] * oxirate / 100.0 + chemicals.[("Ca", "SO3")]
                    "CaSO3*(1/2)H2O", chemicals.[("Ca", "SO2")] * (1.0 - oxirate / 100.0)
                    "CaCO3", kmolTotalCaCO3 - csmCaCO3
                    "CaCl2", chemicals.[("Ca", "HCl")]
                    "CaF2", chemicals.[("Ca", "HF")]
                    "MgSO4", chemicals.[("Mg", "SO2")]
                    "MgCO3", kmolTotalMgCO3 - avlMgCO3
                    "MgCl2", chemicals.[("Mg", "HCl")]
                    "MgF2", chemicals.[("Mg", "HF")] ]
    
    let passive = 
        Map.ofSeq 
            [ "H2O", -(kmolRemoval.HCl + kmolRemoval.HF) / 2.0
              "O2", (kmolRemoval.SO2 - producedSolids.["CaSO3*(1/2)H2O"]) / 2.0
              
              "CO2", 
              -(kmolRemoval.SO2 + kmolRemoval.SO3 + kmolRemoval.HCl / 2.0 + kmolRemoval.HF / 2.0) ]
    
    //转换为质量
    let passive = 
        dict [ for kvp in passive do
                   let nm, kmol = kvp.Key, kvp.Value
                   yield nm, kmol * MaterialProperties.molar nm ]
    
    let produce = 
        dict [ for kvp in producedSolids do
                   let nm, kmol = kvp.Key, kvp.Value
                   yield nm, kmol * MaterialProperties.molar nm
               yield "inerts", ls.["inerts"]
               yield "ash", removal.ash ]

    let    limestone = 
            { Liquid.zero with
                CaCO3 = ls.["CaCO3"]
                MgCO3 = ls.["MgCO3"]
                inerts = ls.["inerts"]
                H2O = ls.["H2O"]
            }
        
    let    removal = 
            { removal with
                H2O = passive.["H2O"]
                O2 = passive.["O2"]
                CO2 = passive.["CO2"]
            }

    let reactHeat = limestoneReactHeat removal

    let productA = dissolve {
            ``CaSO4*2H2O`` = produce.["CaSO4*2H2O"]
            ``CaSO3*(1/2)H2O`` = produce.["CaSO3*(1/2)H2O"]
            ``CaCO3`` = produce.["CaCO3"]
            ``CaCl2`` = produce.["CaCl2"]
            ``CaF2`` = produce.["CaF2"]
            ``MgSO4`` = produce.["MgSO4"]
            ``MgCO3`` = produce.["MgCO3"]
            ``MgCl2`` = produce.["MgCl2"]
            ``MgF2`` = produce.["MgF2"]
            ``inerts`` = produce.["inerts"]
            ``ash`` = produce.["ash"]
        }

    { 
        limestone = limestone
        removal = removal
        productA = productA
        reactHeat = reactHeat
        tss = LiquidPerf.tss productA
        rtuSr = rtuSr
        rtuGrind = rtuGrind
    }





