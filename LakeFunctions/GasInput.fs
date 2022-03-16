namespace LakeFunctions

open Cuisl

module GasInput = 
    (*English 单位*)

    /// loading,lb/MBTU,Gas!I37
    let FlyashExpectedRemoval(loading) = 
        //kg/GJ = 2.3258767121265 lb/MBTU
        let a = 2.425896254
        let b = 6.440939038
        let c = -2.804358355
        exp (a + b * loading ** 2.5 + c * sqrt loading * log loading) |> min 95.0
    
    /// loading,lb/MBTU,Gas!I37
    let FlyashGuaranteedRemoval(loading) = 
        let a = 10.07461166
        let b = -0.227108544
        square (a + b * square (log loading)) |> min 90.0
    
    (*Metric 单位*)


    /// <summary>
    /// 计算吸收塔内对烟气中灰的效率,%
    /// </summary>
    /// <param name="loading">mg/kJ</param>
    let AshExpectedRemoval(loading) = 
        exp 
            (2.425896254 + 0.1680412543e-5 * loading ** 2.5 + 0.8200901004 * sqrt (loading) 
             - 0.1352467552 * sqrt (loading) * log (loading)) |> min 95.0
    
    /// <summary>
    /// 计算吸收塔内对烟气中灰的保证效率,%
    /// </summary>
    /// <param name="loading">mg/kJ</param>
    let AshGuaranteedRemoval(loading) = 
        let x = log loading
        square (-0.227108544 * square x + 2.754217184 * x + 1.724295811) |> min 90.0
