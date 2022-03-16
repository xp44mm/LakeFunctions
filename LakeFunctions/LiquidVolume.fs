module LakeFunctions.LiquidVolume

type liquidProperties =
    {
        tss       :float
        tds       :float
        fw        :float
        tf        :float
        concCl : float
        solids    :float
        sgSolution:float
        sgSolid   :float
        sg  :float
        density   :float
        volume    :float
    }

let calc(liquid: Liquid) =
    let tss = LiquidPerf.tss liquid
    let tds = LiquidPerf.tds liquid
    let fw = LiquidPerf.fw liquid
    let tf = LiquidPerf.tf liquid
    let concCl = liquid.``Cl-`` / fw * 1e6

    let solids = LiquidPerf.solids liquid
    let sgSolution = LiquidPerf.sgSolution liquid
    let sgSolid = LiquidPerf.suspendedSpecificGravity liquid
    let sg = 100. / ((solids / sgSolid) + ((100. - solids) / sgSolution))

    let density = sg * 1e3
    let volume = tf / density

    {
        tss        = tss
        tds        = tds
        fw         = fw
        tf         = tf
        concCl = concCl
        solids     = solids
        sgSolution = sgSolution
        sgSolid    = sgSolid
        sg   = sg
        density    = density
        volume     = volume
    }