namespace Test

open LakeFunctions
open Xunit
open Xunit.Abstractions
open System.Linq

type AbsorberWithUndertrayTest(output: ITestOutputHelper) = 
    [<Fact>]
    member this.absInletWithUnderspray() = 
        let gasFlow = 774491.803693137 // m3/hr
        let diameter = 8.6 // m
        let undertrayPumpFlow = 3200.0
        let aboveFlow = 9600.0
        let inletVelocity = 13.9123797050294
        let inletTemperature = 164.252826617868
        let saturTemperature = 54.1216871361668
        let exp_inletDP = 466.207539872565 // total
        let exp_underSprayDP = 116.730483380628 // under spray
        let inletDP0Underspray = 349.4770565 // inlet

        let actual = 
            let absorber = AbsorberCalc.absorberPrecalc gasFlow diameter
            absorberWithUndertray.absInletWithUnderspray absorber undertrayPumpFlow aboveFlow inletVelocity inletTemperature saturTemperature

        Assert.Equal(inletDP0Underspray, actual.inletDP0Underspray, 3)
        Assert.Equal(exp_underSprayDP, actual.dp, 3)
    
    [<Fact>]
    member this.absorberWithUndertray() = 
        let absorberInput = 
            {gasFlow = 775077.759805474
             diameter = 8.6
             pumpFlow = 3200.0
             sprays = [|"Std on"; "Std on"; "Std on"; "None"; "None"; "None"|]
             openArea = 36.0
             undersprayFlow = 3200.0
             inletVelocity = 13.9123797050294
             saturTemperature = 54.1216871361668
             inletTemperature = 164.252826617868 }: absorberWithUndertray.AbsorberWithUndertrayInput

        let exp_operatingHeaders = 3
        let exp_totalFlow = 12800.0
        let exp_flux = 220.355140246215
        let exp_lg = 16.5144720495078
        let exp_stdLg = 20.0819402280734
        let exp_spraysDp = 734.609738579155
        let exp_spraysStdDp = 555.708878501739
        let exp_trayPressureDrop = 746.804153854137
        let exp_trayStdPressureDrop = 643.074952919757
        let exp_trayNg = 0.903687310263304
        let exp_trayMaxVel = 9.15564019680677
        let exp_undersprayPressureDrop = 116.730483380628
        let exp_inletPressureDrop = 349.477056491937
        let exp_stdDp = 1315.51431474495

        let ac = absorberWithUndertray.absorberWithUndertray absorberInput

        Assert.Equal(exp_operatingHeaders, ac.operatingHeaders)
        Assert.Equal(exp_totalFlow, ac.totalFlow)
        Assert.Equal(exp_flux, ac.flux, 9)
        Assert.Equal(exp_lg, ac.lg, 4)
        Assert.Equal(exp_stdLg, ac.stdLg, 4)
        Assert.Equal(exp_spraysDp, ac.spraysDp, 3)
        Assert.Equal(exp_spraysStdDp, ac.spraysStdDp, 3)
        Assert.Equal(exp_trayMaxVel, ac.trayMaxVel, 3)
        Assert.Equal(exp_trayPressureDrop, ac.trayPressureDrop, 3)
        Assert.Equal(exp_trayStdPressureDrop, ac.trayStdPressureDrop, 3)
        Assert.Equal(exp_trayNg, ac.trayNg,5)
        Assert.Equal(exp_undersprayPressureDrop, ac.undersprayPressureDrop,3)
        Assert.Equal(exp_inletPressureDrop, ac.inletPressureDrop, 3)
        Assert.Equal(exp_stdDp, ac.stdDp, 3)

