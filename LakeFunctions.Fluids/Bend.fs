namespace LakeFunctions.Fluid

open MathNet.Numerics

module Bend = 
    
    ///
    let 急转弯头基本阻力系数 = 
        //use db = new LakeContext()
        //query {
        //    for c in db.急转弯头基本阻力系数 do
        //    select (c.R,c.Kz) //x,y
        //} |> Cuisl.MathNet.interpolate
        let xs,ys = 
            Lake.Fluid.急转弯头基本阻力系数.DataRecords
            |> Array.map(fun c -> c.r,c.kz)
            |> Array.sortBy fst
            |> Array.unzip

        Interpolate.Linear(xs, ys).Interpolate

    ///
    let 缓转弯头基本阻力系数 = 
        //use db = new LakeContext()
        //query {
        //    for c in db.缓转弯头基本阻力系数 do
        //        select (c.R,c.Kz) //x,y
        //        }
        //|> Cuisl.MathNet.interpolate
        let xs,ys = 
            Lake.Fluid.缓转弯头基本阻力系数.DataRecords
            |> Array.map(fun c -> c.r,c.kz)
            |> Array.sortBy fst
            |> Array.unzip

        Interpolate.Linear(xs, ys).Interpolate



    ///此函数需要检查，可能有误
    let 变截面急转弯头基本阻力系数 = 
        let datasource = 
            Lake.Fluid.变截面急转弯头基本阻力系数.DataRecords
            |> Array.map(fun c -> c.r,c.f,c.kz)
            |> Array.sort

            //use db = new LakeContext()
            //query {
            //    for c in db.变截面急转弯头基本阻力系数 do
            //        select (c.R,c.F,c.Kz) 
            //} |> Seq.toArray
        
        let rs = 
            datasource
            |> Seq.map (fun (r, _, _) -> r)
            |> Seq.distinct
            |> Seq.toArray
        
        let steps = 
            let si = new Interpolation.StepInterpolation(rs, rs)
            si.Interpolate
        
        let splines = 
            datasource
            |> Seq.groupBy (fun (r, _, _) -> r)
            |> Seq.map (fun (r, seq) -> 
                   let xs, ys = 
                       seq
                       |> Seq.map (fun (_, f, kz) -> f, kz)
                       |> Seq.toArray
                       |> Array.unzip
                   
                   let spline = Interpolate.Linear(xs, ys)
                   r, spline.Interpolate)
            |> Map.ofSeq
        
        fun (r : float) (f : float) -> 
            let r = steps r
            splines.[r] f
    
    let 变截面尖角弯头基本阻力系数 = 
        //use db = new LakeContext()
        //query {
        //    for c in db.变截面尖角弯头基本阻力系数 do
        //        select (c.F,c.Kz) //x,y
        //        }
        //|> Cuisl.MathNet.interpolate
        let xs,ys = 
            Lake.Fluid.变截面尖角弯头基本阻力系数.DataRecords
            |> Array.map(fun c -> c.f,c.kz)
            |> Array.sortBy fst
            |> Array.unzip

        Interpolate.Linear(xs, ys).Interpolate

        

