namespace Test
open LakeFunctions

open Xunit
open Xunit.Abstractions

type SaturWaterTest(output: ITestOutputHelper) =

    [<Fact>]
    member this.saturPressure() =
        let data =
            [0.0, 0.0006112
             9.5, 0.00118795
             19.0, 0.0021975
             28.5, 0.00389605435854401
             38.0, 0.00664785895986433
             47.5, 0.0109494033895078
             57.0, 0.0174244
             66.5, 0.0268426691357423
             76.0, 0.0402287131782946
             85.5, 0.0589669048020219
             95.0, 0.084533
             104.5, 0.118796413721414]
        for t, p in data do
            let p1 = SaturWater.saturPressure t
            Assert.Equal(p, p1, 6)

    [<Fact>]
    member this.saturTemprature() =
        let data =
            [
                0.01,45.7988
                0.1 ,99.634
                0.15,111.341157844483
                0.5 ,151.867
                1.0 ,179.916
            ]

        for p, expected in data do
            let t = SaturWater.saturTemprature p
            Assert.Equal(expected, t, 6)

    [<Fact>]
    member this.latentHeat() =
        let data =
            [
                0.01, 2392.0
                0.1 , 2257.6
                0.15, 2226.27088977741
                0.5 , 2108.2
                1.0 , 2014.8
            ]

        for p, expected in data do
            let l = SaturWater.latentHeat p
            Assert.Equal(expected, l, 6)