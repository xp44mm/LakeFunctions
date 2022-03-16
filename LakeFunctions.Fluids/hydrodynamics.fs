namespace LakeFunctions.Fluid

open MathNet.Numerics

module Hydrodynamics =

    ///动力粘度: DV, Pa*s
    ///含固量, %
    let dynamicViscosity = 
        //use db = new LakeContext()
        //query {
        //    for c in db.浆液动力粘度 do
        //    select (c.含固量,c.动力粘度)
        //} |> Cuisl.MathNet.interpolate
        let xs,ys = 
            Lake.Fluid.浆液动力粘度.DataRecords
            |> Array.map(fun c -> c.含固量,c.动力粘度)
            |> Array.sortBy fst
            |> Array.unzip

        Interpolate.Linear(xs, ys).Interpolate


    ///kinematical viscosity: KV, m2/s
    ///density: kg/m3
    let kViscosity dv dens: float = dv/dens

    ///diameter:m
    ///velocity:m/s
    ///viscosity:m2/s
    let re diameter velocity kViscosity:float = diameter * velocity / kViscosity

    ///diameter:mm
    let delta diameter = 0.2 / diameter

    let lambda re delta =
        match re with
        | _ when re < 2000.      -> 64./re
        | _ when re < 560./delta -> 0.11 * (delta + 68. / re) ** 0.25
        | _                      -> 1. / pown (1.14 - 2. * log10(delta))  2

