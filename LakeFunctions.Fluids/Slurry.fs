namespace LakeFunctions.Fluid

type Slurry( density   // 1000.0 kg/m3
           , solids    // 0.0    %
           , flow      // 20.0   m3/hr
           , diameter  // 100.0  mm
    ) =

    let area   = System.Math.PI / 4.0 * (diameter * 1e-3)**2.0
    let delta  = 0.2 / diameter
    let dv     = Hydrodynamics.dynamicViscosity solids
    let kv     = dv / density
    let veloc  = (flow / 3600.0) / area
    let head   = veloc**2.0 / 2.0 / 9.807
    let re     = Hydrodynamics.re (diameter * 1e-3) veloc kv
    let lambda = Hydrodynamics.lambda re delta

    member __.Area    = area
    member __.Delta   = delta
    member __.Dv      = dv

    ///运动粘度，m/s2
    member __.Kv      = kv
    member __.Veloc   = veloc

    ///动压头,m
    member __.Head    = head
    member __.Re      = re
    member __.Lambda  = lambda
