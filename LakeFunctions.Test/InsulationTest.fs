namespace Test
open LakeFunctions
open Xunit
open Xunit.Abstractions

type InsulationTest(output: ITestOutputHelper) = 
    
    [<Fact>]
    member this.常年最大散热密度() = 
        let data =
            [
                50.,58.
                75.,75.5
            ]

        for t,expected in data do
            let actual = Insulation.常年最大散热密度 t
            Assert.Equal(expected, actual)
    

    [<Fact>]
    member this.winterHeatloss() = 
        let data =
            [
                50.,116.
                75.,139.5
            ]

        for t,expected in data do
            let actual = Insulation.winterHeatloss t
            Assert.Equal(expected, actual)
    
    [<Fact>]
    member this.flatOuterTemperature()=
        let heatloss = 116. // 散热密度，W/m2
        let innerTemperature = 50. // 温度，℃
        let material = "岩棉" //保温材料名称
        let thick = 50e-3 //保温厚度, m

        let x = LakeFunctions.Insulation.flatOuterTemperature heatloss innerTemperature material thick
        output.WriteLine(sprintf "%A" x)

    [<Fact>]
    member this.``圆筒是自然对数``()=
        let d0 = 25.
        let delta = 8.
        let d1 = d0 + 2. * delta
        let res = d1* log (d1/d0)

        output.WriteLine(sprintf "%A" res)

    [<Fact>]
    member this.pipeOuterTemperature()=
        let heatloss = 116. // 散热密度，W/m2
        let innerTemperature = 150. // 温度，℃
        let material = "岩棉" //保温材料名称
        let innerDiameter = 108e-3
        let thick = 40e-3 //保温厚度, m

        let x = LakeFunctions.Insulation.pipeOuterTemperature heatloss innerTemperature material innerDiameter thick
        output.WriteLine(sprintf "%A" x)
