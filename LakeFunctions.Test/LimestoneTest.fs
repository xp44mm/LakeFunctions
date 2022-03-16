namespace Test
open LakeFunctions

open Xunit
open Xunit.Abstractions

type LimestoneTest(output: ITestOutputHelper) = 
    
    [<Fact>]
    member this.limestoneReactHeat() = 
        let gas = 
            {
                Gas.zero with
                    H2O= -28.1444870985153
                    O2=1021.49118160629
                    CO2= -2894.28637820587
                    SO2=4102.1856
                    SO3=13.607772
                    HCl=47.3005
                    HF=36.556
            }
        let expected = 22159712.2193597
        let actual = Limestone.limestoneReactHeat gas

        Assert.Equal(expected,actual,6)

    [<Fact>]
    member this.limeReactHeat() = 
        let gas = 
            {
                Gas.zero with
                    H2O= -28.1444870985153
                    O2=1021.49118160629
                    CO2= -2894.28637820587
                    SO2=4102.1856
                    SO3=13.607772
                    HCl=47.3005
                    HF=36.556
            }
        let expected = 29885245.5039651
        let actual = Limestone.limeReactHeat gas

        Assert.Equal(expected,actual,6)

    

