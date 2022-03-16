module LakeFunctions.TowerSize
open Cuisl

type SizeInput() = 
    member val diameter = 0.0 with get,set
    member val high = 0.0 with get,set
    member val injectElevation = 0.0 with get,set
    member val flare = 0.0 with get,set
    member val flareHeight = 0.0 with get,set

type loopResult =
    {
        holdup:float
        dll:float
        ddll:float
        injectDeep:float
        slurryVolume:float
    }

let circleArea d = System.Math.PI / 4.0 * square d

//高操作液位
let spaceHoll = 0.3048

type TowerSize(size:SizeInput) = 
    member __.diameter = size.diameter 
    member __.high = size.high
    member __.injectElevation = size.injectElevation

    member size.holl = size.high - spaceHoll
    member size.maxInjectDeep = size.holl - size.injectElevation
    
    member size.crystal = size.area * size.injectElevation
    member size.area = circleArea size.diameter

    member size.designate(holdup) =
        let dll = size.holl - holdup
        let injectDeep = size.maxInjectDeep - holdup
        let slurryVolume = size.area * dll
        {
            holdup = holdup
            dll = dll
            ddll = size.diameter
            injectDeep = injectDeep
            slurryVolume = slurryVolume
        }

type FlareTowerSize(size:SizeInput) =
    member __.diameter = size.diameter 
    member __.high = size.high
    member __.injectElevation = size.injectElevation
    member __.flare = size.flare
    member __.flareHeight = size.flareHeight

    member size.holl = size.high - spaceHoll
    member size.maxInjectDeep = size.holl - size.injectElevation

    member size.crystal = circleArea size.tankDiameter * size.injectElevation
    
    member size.ctan = size.flare / size.flareHeight
    member size.dholl = size.diameter + 2.0 * size.ctan * spaceHoll

    member size.tankHeight = size.high - size.flareHeight
    member size.tankDiameter = size.diameter + 2.0 * size.flare
    member size.tankVolume = circleArea size.tankDiameter * size.tankHeight

    member size.designate(holdup) =
        let dll = size.holl - holdup
        let injectDeep = size.maxInjectDeep - holdup
        let ddll = size.dholl + 2.0 * size.ctan * holdup
        
        let coneVolume =
            let h = dll - size.tankHeight
            let d1 = ddll
            let d2 = size.tankDiameter
            System.Math.PI * h * (square d1 + square d2 + d1 * d2) / 12.0

        let slurryVolume = size.tankVolume + coneVolume
        {
            holdup = holdup
            dll = dll
            ddll = ddll
            injectDeep = injectDeep
            slurryVolume = slurryVolume
        }

    member size.holdupRec(volume) =
        let ctan = size.ctan
        let d1 = size.dholl
        let v = volume

        if ctan > 0.0 then 
            //let hh = size.dholl / 2.0 / size.ctan //圆锥高度
            //cbrt (cubic hh + 3.0 / System.Math.PI * volume / square size.ctan) - hh
            (System.Math.PI ** (2. / 3.) * (System.Math.PI * d1 ** 3. + 24. * ctan * v) ** (1. / 3.) - System.Math.PI * d1) / System.Math.PI / ctan / 2.
        else
            v / circleArea d1

