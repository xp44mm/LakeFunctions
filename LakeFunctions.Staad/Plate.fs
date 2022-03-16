namespace LakeFunctions.Ribs

//open LakeFunctions

/// <summary>
/// 用于计算加固肋中心间距
/// </summary>
/// <param name="ela">弹性模量,GPa</param>
/// <param name="t">板厚,mm</param>
/// <param name="span">加固肋间距,mm</param>
type Plate(t, ela, span) =

    /// 板频率,Hz
    member __.Freq = Checker.Plate.freq t ela span

    /// <summary>
    /// 板应力, MPa
    /// </summary>
    /// <param name="q">荷载, kPa</param>
    member __.Stress q = Checker.Plate.stress t span q

    /// <summary>
    /// 板挠度，无量纲，大于120合格
    /// </summary>
    /// <param name="q">荷载, kPa</param>
    member __.Deflection q = Checker.Plate.deflection t ela span q