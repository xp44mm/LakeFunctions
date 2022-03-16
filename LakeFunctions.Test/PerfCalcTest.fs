namespace Test

open LakeFunctions
open Xunit
open Xunit.Abstractions

type trayPressureDropTestData = {
    lg : float
    trayVel : float
    exp_trayDp : float
}

type PerfCalcTest(output: ITestOutputHelper) = 
    
    [<Fact>]
    member this.limestoneReactHeat() = 
        let flux = 275.4439253 //(m3/hr)/m2 
        let percent = 20.0 // %
        let actual = PerfCalc.inletDP flux percent
        let expected = 1096.46767453441
        Assert.Equal(expected, actual, 6)
    
    [<Fact>]
    member this.inletDP() = 
        let flux = 275.4439253 //(m3/hr)/m2 
        let percent = 20.0 // %
        let actual = PerfCalc.inletDP flux percent
        let expected = 1096.46767453441
        Assert.Equal(expected, actual, 6)

    [<Fact>]
    member this.trayPressureDrop() =
        let test data =
            let lg = data.lg
            let trayVel = data.trayVel
            let exp_trayDp = data.exp_trayDp
            let trayDp = PerfCalc.trayPressureDrop lg trayVel
            Assert.Equal(exp_trayDp, trayDp, 3)

        let data = {
            lg = 12.3952247843033
            trayVel = 10.2878586445444
            exp_trayDp = 746.30206251576
        }

        let datastd = {
            lg = 15.0614551710551
            trayVel = 8.46666666666667
            exp_trayDp = 643.074896540523
        }

        test datastd
