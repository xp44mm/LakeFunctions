﻿namespace Test
open LakeFunctions.Fluid

open Xunit
open Xunit.Abstractions

type FluidTest(output: ITestOutputHelper) = 
    
    [<Fact>]
    member this.急转弯头基本阻力系数() = 
        let data =
            [
                0.1,0.85
                0.2,0.54
                0.3,0.38
            ]

        for r,expected in data do
            let kz = Bend.急转弯头基本阻力系数 r
            Assert.Equal(expected, kz)
    
    
    [<Fact>]
    member this.缓转弯头基本阻力系数() = 
        let data =
            [
                0.5,1.55
                0.6,1.0
                0.7,0.69
            ]

        for r,expected in data do
            let kz = Bend.缓转弯头基本阻力系数 r
            Assert.Equal(expected, kz)
    
    [<Fact>]
    member this.变截面急转弯头基本阻力系数() = 
        let data =
            [
                0.1,0.5,0.59
                0.1,0.6,0.65
                0.1,0.7,0.7
                0.1,0.8,0.75
                0.1,0.9,0.78
                0.1,1.0,0.81
                0.1,1.1,0.85
                0.1,1.2,0.87
                0.1,1.3,0.9
                0.1,1.4,0.92
                0.1,1.5,0.94
                0.1,1.6,0.96
                0.1,1.7,0.97
                0.1,1.8,0.98
                0.1,1.9,0.99
                0.1,2.0,1.0
                0.15,0.5,0.4
                0.15,0.6,0.48
                0.15,0.7,0.55
                0.15,0.8,0.6
                0.15,0.9,0.65
                0.15,1.0,0.68
                0.15,1.1,0.71
                0.15,1.2,0.75
                0.15,1.3,0.77
                0.15,1.4,0.8
                0.15,1.5,0.81
                0.15,1.6,0.84
                0.15,1.7,0.86
                0.15,1.8,0.88
                0.15,1.9,0.89
                0.15,2.0,0.9
                0.2,0.5,0.24
                0.2,0.6,0.31
                0.2,0.7,0.37
                0.2,0.8,0.44
                0.2,0.9,0.49
                0.2,1.0,0.54
                0.2,1.1,0.57
                0.2,1.2,0.6
                0.2,1.3,0.65
                0.2,1.4,0.67
                0.2,1.5,0.7
                0.2,1.6,0.72
                0.2,1.7,0.74
                0.2,1.8,0.76
                0.2,1.9,0.78
                0.2,2.0,0.79
                0.3,0.5,0.13
                0.3,0.6,0.18
                0.3,0.7,0.22
                0.3,0.8,0.27
                0.3,0.9,0.32
                0.3,1.0,0.36
                0.3,1.1,0.4
                0.3,1.2,0.44
                0.3,1.3,0.47
                0.3,1.4,0.5
                0.3,1.5,0.54
                0.3,1.6,0.57
                0.3,1.7,0.6
                0.3,1.8,0.62
                0.3,1.9,0.64
                0.3,2.0,0.66
                0.4,0.5,0.09
                0.4,0.6,0.13
                0.4,0.7,0.18
                0.4,0.8,0.23
                0.4,0.9,0.26
                0.4,1.0,0.3
                0.4,1.1,0.34
                0.4,1.2,0.38
                0.4,1.3,0.42
                0.4,1.4,0.45
                0.4,1.5,0.48
                0.4,1.6,0.51
                0.4,1.7,0.54
                0.4,1.8,0.56
                0.4,1.9,0.58
                0.4,2.0,0.6
                1.0,0.5,0.05
                1.0,0.6,0.09
                1.0,0.7,0.13
                1.0,0.8,0.17
                1.0,0.9,0.21
                1.0,1.0,0.25
                1.0,1.1,0.3
                1.0,1.2,0.34
                1.0,1.3,0.36
                1.0,1.4,0.4
                1.0,1.5,0.43
                1.0,1.6,0.45
                1.0,1.7,0.48
                1.0,1.8,0.51
                1.0,1.9,0.53
                1.0,2.0,0.55
            ]

        for r,f,expected in data do
            let kz = Bend.变截面急转弯头基本阻力系数 r f
            Assert.Equal(expected, kz)
    
    [<Fact>]
    member this.变截面尖角弯头基本阻力系数() = 
        let data =
            [
                0.1,0.5
                0.2,0.51
                0.3,0.54
                0.4,0.58
                0.5,0.65
                0.9,1.25
                1.0,1.4
                1.1,1.35
                1.2,1.25
                1.5,1.1
                1.8,1.05
            ]

        for f,expected in data do
            let kz = Bend.变截面尖角弯头基本阻力系数 f
            Assert.Equal(expected, kz)

    [<Fact>]
    member this.roundBendAngleCorrect() = 
        let data =
            [
                0.0,0.0
                30.0,0.45
                60.0,0.78
                90.0,1.0
                120.0,1.9
                150.0,2.6
                180.0,3.0
            ]

        for a,expected in data do
            let kz = BendCorrect.roundBendAngleCorrect a
            Assert.Equal(expected, kz)


    [<Fact>]
    member this.sharpBendAngleCorrect() = 
        let data =
            [
                0.0,0.0
                30.0,0.15
                60.0,0.4
                90.0,1.0
                120.0,1.15
                150.0,1.28
                180.0,1.4
            ]

        for a,expected in data do
            let kz = BendCorrect.sharpBendAngleCorrect a
            Assert.Equal(expected, kz)

    [<Fact>]
    member this.sharpBendShapeCorrect() = 
        let data =
            [
                0.0,1.2
                0.3,1.1
                1.0,1.0
                2.0,0.9
                4.0,0.8
                6.0,0.73
                8.0,0.69
            ]

        for a,expected in data do
            let kz = BendCorrect.sharpBendShapeCorrect a
            Assert.Equal(expected, kz)

    [<Fact>]
    member this.smallRoundBendShapeCorrect() = 
        let data =
            [
                0.3,1.29
                0.5,1.19
                1.0,1.0
                1.5,0.92
                2.0,0.86
                2.5,0.85
                3.0,0.85
                4.0,0.9
                5.0,0.95
                6.0,0.97
                8.0,1.0
            ]

        for a,expected in data do
            let kz = BendCorrect.smallRoundBendShapeCorrect a
            Assert.Equal(expected, kz)
    
    [<Fact>]
    member this.largeRoundBendShapeCorrect() = 
        let data =
            [
                0.3,1.7
                0.5,1.45
                1.0,1.0
                2.0,0.48
                3.0,0.4
                4.0,0.43
                5.0,0.49
                6.0,0.54
                8.0,0.59
            ]

        for a,expected in data do
            let kz = BendCorrect.largeRoundBendShapeCorrect a
            Assert.Equal(expected, kz)

    [<Fact>]
    member this.dynamicViscosity() = 
        let data =
            [
                0.0,0.0018
                10.0,0.005
                20.0,0.009
                30.0,0.015
                50.0,0.0345
                60.0,0.045
            ]

        for a,expected in data do
            let kz = Hydrodynamics.dynamicViscosity a
            Assert.Equal(expected, kz)

    [<Fact>]
    member this.虾米弯阻力系数() = 
        let data =
            [
                0.5,0.89
                1.6,0.31
                2.0,0.21
                2.5,0.16
                3.0,0.13
                4.0,0.115
                10.5,0.11
            ]

        for a,expected in data do
            let kz = MiterElbow.zeta a
            Assert.Equal(expected, kz)

    [<Fact>]
    member this.孔板Tao值() = 
        let data =
            [
                0.0,1.5
                0.1,1.3
                0.15,1.25
                0.2,1.22
                0.25,1.2
                0.3,1.18
                0.4,1.1
                0.6,0.84
                0.8,0.42
                1.0,0.24
                1.2,0.16
                1.6,0.07
                2.0,0.02
                2.4,0.0
                100.0,0.0
            ]

        for a,expected in data do
            let kz = Orrifice.tao a
            Assert.Equal(expected, kz)
