namespace Test
open LakeFunctions

open Xunit
open Xunit.Abstractions

type MaterialPropertiesTest(output: ITestOutputHelper) = 

    [<Fact>]
    member this.molar() = 
        let data =
            [
                "H2O",18.01528
                "O2",31.9988
                "N2",28.0134
                "CO2",44.0095
                "SO2",64.0638
                "SO3",80.0632
                "HCl",36.46094
                "HF",20.0063432
            ]

        for name,expected in data do
            let actual = MaterialProperties.molar name
            //output.WriteLine("{0},{1}",name,actual)
            Assert.Equal(expected, actual)

    [<Fact>]
    member this.specificGravity() = 
        let data =
            [
                "CaSO4*2H2O",2.3
                "CaSO3*(1/2)H2O",2.3
                "CaCO3",2.71
                "MgSO4",2.66
                "MgCO3",2.96
                "Inerts",2.3
                "Ash",2.3
                "CaF2",2.3
                "MgF2",2.3
            ]

        for name,expected in data do
            let actual = MaterialProperties.specificGravity name
            Assert.Equal(expected, actual)

    [<Fact>]
    member this.solubility() = 
        let data =
            [
                "CaF2",0.00002
                "MgF2",0.00002
                "MgSO4",0.55
            ]

        for name,expected in data do
            let actual = MaterialProperties.solubility name
            Assert.Equal(expected, actual)
