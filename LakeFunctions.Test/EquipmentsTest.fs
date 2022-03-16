namespace Test
open LakeFunctions

open Xunit
open Xunit.Abstractions

type EquipmentsTest(output: ITestOutputHelper) = 
    
    [<Fact>]
    member this.箱子重量() = 
        let data =
            [
                5.,6.,10.7123400748891
                3.5,4.2,5.24904663669564
            ]

        for d,h,res in data do
            let actual = Equipments.箱子重量 d h
            Assert.Equal(res,actual,12)


    [<Fact>]
    member this.搅拌器功率() = 
        let data =
            [
                59.,4.
                120.,11.
            ]

        for volume,powern in data do
            let actual = Equipments.搅拌器功率 volume
            Assert.Equal(powern,actual)

    [<Fact>]
    member this.泵效率() = 
        let data =
            [
                10. , 20.0
                30. , 20.0
                100., 30.0
            ]

        for f,expected in data do
            let actual = Equipments.泵效率 f
            //output.WriteLine(sprintf "%A" actual)
            Assert.Equal(expected,actual)
            

    [<Fact>]
    member this.罗茨风机功率() = 
        let data =
            [
                10.   , 37.0
                30.   , 90.0
                100.  , 355.0
            ]

        for f,expected in data do
            let actual = Equipments.罗茨风机功率 f 98.0
            //output.WriteLine(sprintf "%A" actual)
            Assert.Equal(expected,actual)

