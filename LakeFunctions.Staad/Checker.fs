namespace LakeFunctions.Ribs.Checker

/// len , 长度, mm;
/// ela , 弹性模量, GPa;
/// ix  , cm4;
/// zmin, 截面模量, cm3;
/// wt  , 重量, kg/m;
/// q   , kN/m;
module Fixed =
    ///频率,Hz
    let freq len ela ix wt = 1.8572 / (len / 1000.)**2.0 * sqrt(10. * ela * ix / wt)

    ///应力, MPa
    let stress len zmin q = q * (len / 1000.0)**2.0 / 12. / zmin * 1000.

    ///挠度, 无量纲, 大于400合格
    let deflection len ela ix q =
        let mm = 250. / 384. * q * (len / 1000.)**4.0 / ela / ix * 1000.
        len / mm

module Pinned =
    ///肋频率, Hz
    let freq len ela ix wt = 0.5 * System.Math.PI / (len / 1000.)**2.0 * sqrt(10. * ela * ix / wt)

    ///肋应力, MPa
    let stress len zmin q = q * (len / 1000.)**2.0 / 8. / zmin * 1000.

    ///肋挠度, 无量纲, 大于400合格
    let deflection len ela ix q =
        let mm = 500. / 384. * q * (len / 1000.)**4.0 / ela / ix * 1000.
        len / mm

/// <summary>
/// 用于计算加固肋中心间距
/// </summary>
/// <param name="ela">弹性模量,GPa</param>
/// <param name="t">板厚,mm</param>
/// <param name="span">加固肋间距,mm</param>
module Plate =
    /// 板频率,Hz
    let freq t ela span = 0.76 * t / (span / 1000.)**2.0 * sqrt(ela / 7.85)

    /// <summary>
    /// 板应力, MPa
    /// </summary>
    /// <param name="q">荷载, kPa</param>
    let stress t span q = (span / t)**2.0 * q / 3000.0

    /// <summary>
    /// 板挠度，无量纲，大于120合格
    /// </summary>
    /// <param name="q">荷载, kPa</param>
    let deflection t ela span q =
        let mm = 91. / 64. * q * (span / 100.)**4.0 / t**3.0 / ela
        span / mm