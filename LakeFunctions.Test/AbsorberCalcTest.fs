namespace Test

open LakeFunctions
open Xunit
open Xunit.Abstractions
open System.Linq

type sprayCalcTestData = 
    {
        gasFlow: float // m3/hr
        diameter: float // m
        pumpFlow: float
        sprayType: string
        aboveFlow: float
        exp_headers: int
        exp_liquidFlow: float
        exp_flux: float
        exp_dp: float
        exp_dpstd: float
        exp_lg: float
        exp_lgstd: float}

type AbsorberCalcTest(output: ITestOutputHelper) = 
    
    [<Fact>]
    member this.absorberPrecalc() = 
        let gasFlow = 774491.803693137 // m3/hr
        let diameter = 8.6 // m
        let exp_area = 58.0880481648753 //m2
        let exp_vel = 3.703629112036 //m/s
        let actual = AbsorberCalc.absorberPrecalc gasFlow diameter

        Assert.Equal(gasFlow,actual.gasFlow)
        Assert.Equal(diameter,actual.diameter)
        Assert.Equal(exp_area, actual.area, 5)
        Assert.Equal(exp_vel, actual.vel, 5)
    
    [<Fact>]
    member this.sprayCalc() = 
        let test(data: sprayCalcTestData) = 
            let absorber = AbsorberCalc.absorberPrecalc data.gasFlow data.diameter 
            let res = AbsorberCalc.sprayCalc absorber data.pumpFlow data.sprayType data.aboveFlow

            Assert.Equal(data.exp_headers, res.headers)
            Assert.Equal(data.exp_liquidFlow, res.liquidFlow, 5)
            Assert.Equal(data.exp_flux, res.flux, 5)
            Assert.Equal(data.exp_dp, res.dp, 3)
            Assert.Equal(data.exp_dpstd, res.dpstd, 3)
            Assert.Equal(data.exp_lg, res.lg, 5)
            Assert.Equal(data.exp_lgstd, res.lgstd, 5)
        
        let topest = 
            {gasFlow = 774491.803693137
             diameter = 8.6
             pumpFlow = 3200.0
             sprayType = "Std on"
             aboveFlow = 0.0
             exp_headers = 1
             exp_liquidFlow = 3200.0
             exp_flux = 55.0887850615538
             exp_dp = 125.529055806954
             exp_dpstd = 95.0684454283549
             exp_lg = 4.13174159476777
             exp_lgstd = 5.02048505701836}
        
        test topest
        test {topest with aboveFlow = 3200.0
                          exp_liquidFlow = 6400.0
                          exp_flux = 110.177570123108
                          exp_dp = 245.283026619788
                          exp_dpstd = 185.763175551689
                          exp_lg = 8.26348318953554
                          exp_lgstd = 10.0409701140367}
        test {topest with aboveFlow = 6400.0
                          exp_liquidFlow = 9600.0
                          exp_flux = 165.266355184661
                          exp_dp = 362.949898297297
                          exp_dpstd = 274.877257521696
                          exp_lg = 12.3952247843033
                          exp_lgstd = 15.0614551710551}
    
    [<Fact>]
    member this.trayCalc() = 
        let gasFlow = 774491.803693137 // m3/hr
        let diameter = 8.6 // m
        let openArea = 36.0 // %
        
        let above: AbsorberCalc.SprayResult = 
            {headers = 1
             liquidFlow = 9600.0
             flux = 165.266355184661
             dp = 0.0
             dpstd = 0.0
             lg = 12.3952247843033
             lgstd = 15.0614551710551}
        
        let exp_trayMaxVel = 9.15564019680677
        let exp_trayVel = 10.2878586445444
        let exp_trayStdVel = 8.46666666666667
        let exp_trayDp = 746.30206251576
        let exp_trayStdDp = 643.074952919757

        let absorber = AbsorberCalc.absorberPrecalc gasFlow diameter 
        let actual = AbsorberCalc.trayCalc absorber openArea above

        Assert.Equal(exp_trayMaxVel, actual.trayMaxVel, 5)
        Assert.Equal(exp_trayVel, actual.trayVel, 5)
        Assert.Equal(exp_trayStdVel, actual.trayStdVel, 5)
        Assert.Equal(exp_trayDp, actual.trayDp, 3)
        Assert.Equal(exp_trayStdDp, actual.trayStdDp, 3)
    
    [<Fact>]
    member this.absInlet() = 
        let totalFlux = 220.355140246215
        let inletVelocity = 13.9123797050294
        let inletTemperature = 164.252826617868
        let saturTemperature = 54.1216871361668
        let exp_inletDP0Underspray = 349.477056491937
        let exp_tcorrect = 0.748215757238542
        let exp_vcorrect = 0.833360497382708
        let actual = AbsorberCalc.absInlet totalFlux inletVelocity inletTemperature saturTemperature
        Assert.Equal(exp_inletDP0Underspray, actual.inletDP0Underspray, 3)
        Assert.Equal(exp_tcorrect, actual.tcorrect)
        Assert.Equal(exp_vcorrect, actual.vcorrect, 9)
    
    [<Fact>]
    member this.absorberPressureDrop() = 
        let absorberInput = 
            {gasFlow = 774491.803693137
             diameter = 8.6
             pumpFlow = 3200.0
             sprays = [|"Std on"; "Std on"; "Std on"; "None"; "None"; "None"|]
             openArea = 36.0
             inletVelocity = 13.9123797050294
             saturTemperature = 54.1216871361668
             inletTemperature = 164.252826617868}: AbsorberCalc.absorberInput 
        
        let exp_operatingHeaders = 3
        let exp_totalFlow = 9600.0
        let exp_flux = 165.266355184661
        let exp_lg = 12.3952247843033
        let exp_stdLg = 15.0614551710551
        let exp_spraysDp = 733.7621155
        let exp_spraysStdDp = 555.7088785
        let exp_trayMaxVel = 9.15564019680677
        let exp_trayPressureDrop = 746.30206251576
        let exp_trayStdPressureDrop = 643.074952919757
        let exp_inletPressureDrop = 239.5331784956
        let exp_stdDp = 1198.783831
        let ac = AbsorberCalc.absorberPressureDrop absorberInput

        Assert.Equal(exp_operatingHeaders, ac.operatingHeaders)
        Assert.Equal(exp_flux, ac.flux, 9)
        Assert.Equal(exp_lg, ac.lg, 4)
        Assert.Equal(exp_stdLg, ac.stdLg, 4)
        Assert.Equal(exp_spraysDp, ac.spraysDp, 3)
        Assert.Equal(exp_spraysStdDp, ac.spraysStdDp, 3)
        Assert.Equal(exp_trayMaxVel, ac.trayMaxVel, 3)
        Assert.Equal(exp_trayPressureDrop, ac.trayPressureDrop, 3)
        Assert.Equal(exp_trayStdPressureDrop, ac.trayStdPressureDrop, 3)
        Assert.Equal(exp_stdDp, ac.stdDp, 3)
        Assert.Equal(exp_inletPressureDrop, ac.inletPressureDrop, 3)
        Assert.Equal(exp_totalFlow, ac.totalFlow)
