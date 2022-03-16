namespace Test
open LakeFunctions

open Xunit
open Xunit.Abstractions

type IdealGasTest(output: ITestOutputHelper) = 
    
    [<Fact>]
    member this.moleEnthalpy() = 
        let t = 273.15
        let data = 
            [
                "H2O",8944.76082309802
                "O2",7480.12468128818
                "N2",7886.32662886677
                "CO2",8084.11893501314
                "SO2",8956.68224808583
                "SO3",9203.06273793882
                "HCl",8084.48211781469
                "HF",8084.48211781469
             ]
        for fm, expected in data do
            let actual = IdealGas.moleEnthalpy fm t
            Assert.Equal(expected, actual,9)

