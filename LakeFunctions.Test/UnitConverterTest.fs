namespace Test
open LakeFunctions

open Xunit
open Xunit.Abstractions

type UnitConverterTest(output: ITestOutputHelper) = 
    let unitsData = 
        [
            "psi","Pa",6894.757
            "psi","in wc",27.68067143
            "psi","bar",0.06894757
            "psi","in Hg",2.036020375
            "bar","atm",0.986923267
            "bar","Pa",100000.0
            "in Hg","psi",0.491154221
            "in wc","Pa",249.082
            "in wc","psi",0.036126291
            "Pa","in wc",0.004014742
            "Pa","psi",0.000145038
            "gal","m3",0.003785412
            "ft3","m3",0.02831685
            "gal","ft3",7.480519954
            "ft3","gal",0.133680547
            "gal","liter",3.785412
            "m3","gal",264.1720373
            "gpm","m3/hr",0.22712472
            "ft","m",0.3048
            "m","ft",3.280839895
            "lb","kg",0.4535924
            "kg","lb",2.204622476
            "BTU","kJ",1.055
            "kJ","BTU",0.947867299
            "BTU/lb","kJ/kg",2.326
            "kJ/kg","BTU/lb",0.429922614
            "gpm/1000 acfm","l/m3",0.133680547
            "gal/1000 ft3","l/m3",0.133680547
            "l/m3","gal/1000 ft3",7.480519054
            "(gal/hr)/(ft^3/min)","l/m3",2.228009118
            "l/m3","(gal/hr)/(ft^3/min)",0.448831143
            "gpm/ft2","(m3/hr)/m2",2.44475014
            "(m3/hr)/m2","gpm/ft2",0.409039756
        ]
    

    [<Fact>]
    member this.units() = 
        for s,t,expected in unitsData do
            let actual =  UnitConverter.units(s,t)
            Assert.Equal(expected, actual)
