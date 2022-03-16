module LakeFunctions.Staad

open MathNet.Numerics

let 风压高度变化系数 kind height =
    //见《建筑结构荷载规范》161页公式7.2.1
    match kind with
    | "A" -> 1.379 * (height / 10.) ** 0.24
    | "B" -> 1.    * (height / 10.) ** 0.32
    | "C" -> 0.616 * (height / 10.) ** 0.44
    | "D" -> 0.318 * (height / 10.) ** 0.60
    |  _  -> 0.0

///压杆稳定系数
let buckling =
    let xs,ys = 
        Lake.Staad.压杆稳定系数.DataRecords
        |> Array.map(fun c -> c.长细比,c.稳定系数)
        |> Array.sortBy fst
        |> Array.unzip
    let interpolation = Interpolate.Linear(xs, ys)
    fun lambda -> interpolation.Interpolate lambda