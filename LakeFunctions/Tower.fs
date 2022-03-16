module LakeFunctions.Tower
open Cuisl


open TowerSize

type Slurry() =
    member val temperature = 0.0 with get,set

    member val tds = 0.0 with get,set
    member val fw = 0.0 with get,set
    member val solids = 0.0 with get,set
    member val density = 0.0 with get,set

    member val totalPumpFlow = 0.0 with get,set
    member val productAtss = 0.0 with get,set
    member val stoich = 0.0 with get,set
    member val velocity = 0.0 with get,set

type result =
    {
        dll:float
        slurryVolume:float
        solidsMass:float
        minO2:float
        injectPressure: float

        //液体停留时间,min
        retentionTime:float
        //固体停留时间,hr
        residenceTime:float
        pH:float


        //rtuSolids: float
        //rtuPH:float
        //rtuVelocity:float

    }


let postprocess (slurry:Slurry)(absInlet:GasFlow) reactO2 (size:loopResult) =

    let solidsMass = 
        let mass = size.slurryVolume * slurry.density
        mass * slurry.solids / 100.0
    
    let retentionTime = size.slurryVolume / slurry.totalPumpFlow * 60.0
    let residenceTime = solidsMass/ slurry.productAtss
    let pH = PerfCalc.pH(slurry.stoich,residenceTime)

    //最小氧化空气量
    let tds = slurry.tds / slurry.fw * 100.
    let minO2 = TowerCalc.injectMinO2 absInlet.SO2 absInlet.O2 tds reactO2 size.injectDeep
    let injectPressure = absInlet.pressure + 9.80665 * slurry.density * (size.injectDeep + size.holdup)
    //let rtuSolids = Performance.rtu_solids slurry.solids
    //let rtuPH = Performance.rtu_ph pH
    //let rtuVelocity = Performance.rtu_velocity(slurry.velocity,pH)

    {
        dll = size.dll
        slurryVolume = size.slurryVolume
        solidsMass = solidsMass
        minO2 = minO2
        injectPressure = injectPressure

        retentionTime=retentionTime
        residenceTime=residenceTime
        pH=pH
        //rtuSolids = rtuSolids
        //rtuPH=rtuPH
        //rtuVelocity = rtuVelocity
    }

///holdupVol,氧化空气在浆池中的体积,m3;
///dllDiameter,设计液位对应的直径,m;
///oxizone,氧化空气区的浆液体积,m3;
let holdupVol airVolume dllDiameter oxizone = 
    let second = 4.0
    let vel = airVolume / 3600.0 / (System.Math.PI / 4.0 * square dllDiameter) // m/s
    second * vel * oxizone
    
///absInlet.pressure,
///absInlet.SO2,
///absInlet.O2,
///oxiair.H2O,
///oxiair.O2,
///oxiair.N2,
///reactO2 = abs removal.O2,
let flareTower (size:SizeInput)(slurry:Slurry) (absInlet:GasFlow) airVolume reactO2 =
    let size = FlareTowerSize(size)
    let airVolume = airVolume * IdealGas.tcorrect slurry.temperature

    let rec loop holdup =
        let d = size.designate holdup
        
        let airVolume = 
            let p = absInlet.pressure + 9.80665 * slurry.density * d.injectDeep / 2.0
            airVolume * IdealGas.pcorrect p

        let oxizone = d.slurryVolume - size.crystal

        let holdupVol = holdupVol airVolume d.ddll oxizone

        let holdupRec = size.holdupRec holdupVol

        if abs(holdupRec-holdup) > 1e-5 then
            loop holdupRec
        else
            size.designate holdup

    loop 0.0
    |> postprocess slurry absInlet reactO2
    
///absInlet.pressure,
///absInlet.SO2,
///absInlet.O2,
///oxiair.H2O,
///oxiair.O2,
///oxiair.N2,
///reactO2 = abs removal.O2,
let tower (size:SizeInput)(slurry:Slurry) (absInlet:GasFlow) airVolume reactO2 =
    let size = TowerSize(size)
    let airVolume = airVolume * IdealGas.tcorrect slurry.temperature

    let rec loop holdup =
        let result = size.designate holdup

        let airVolume = 
            let p = absInlet.pressure + 9.80665 * slurry.density * result.injectDeep / 2.0
            airVolume * IdealGas.pcorrect p

        let oxizone = result.slurryVolume - size.crystal

        let holdupVol = holdupVol airVolume result.ddll oxizone

        let holdupRec = holdupVol / size.area

        if abs(holdupRec-holdup) > 1e-5 then
            loop holdupRec
        else
            size.designate holdupRec
    loop 0.0
    |> postprocess slurry absInlet reactO2

