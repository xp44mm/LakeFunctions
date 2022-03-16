namespace LakeFunctions.Ribs


///固接加固肋计算公式
type FixedRib(len, ela, sect: LakeFunctions.Sections.CombSection) =

    ///肋频率,Hz
    member __.Freq = Checker.Fixed.freq len ela sect.ix sect.weight

    ///肋应力,MPa
    member __.Stress q = Checker.Fixed.stress len sect.zmin q

    ///肋挠度, 无量纲，大于400合格
    member __.Deflection q = Checker.Fixed.deflection len ela sect.ix q

///铰接加固肋计算公式
type PinnedRib(len, ela, sect: LakeFunctions.Sections.CombSection) =

    ///肋频率,Hz
    member __.Freq = Checker.Pinned.freq len ela sect.ix sect.weight

    ///肋应力,MPa
    member __.Stress q = Checker.Pinned.stress len sect.zmin q

    ///肋挠度, 无量纲，大于400合格
    member __.Deflection q = Checker.Pinned.deflection len ela sect.ix q