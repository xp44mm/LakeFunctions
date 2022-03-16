namespace Test
open LakeFunctions

open Xunit
open Xunit.Abstractions

type MotorTest(output: ITestOutputHelper) = 
    
    [<Fact>]
    member this.powern() = 
        let data =
            [
                140.,160.
                150.,185.
                200.,250.
            ]

        for axis,expected in data do
            let actual = Motor.powern axis
            Assert.Equal(expected, actual)
    
    [<Fact>]
    member this.powernExact() = 
        let data =
            [
                140.0,1.2,185.0
                150.0,1.4,220.0
                200.0,1.3,280.0
            ]

        for axis,safe,expected in data do
            let actual = Motor.powernExact axis safe
            Assert.Equal(expected, actual)


