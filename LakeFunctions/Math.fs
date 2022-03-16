namespace Cuisl
open System
[<AutoOpen>]
module Math =
    /// square x => x**2.0
    let square x : float = x * x

    /// cubic x => x**3.0
    let cubic x : float = x * x * x

    /// quartic x => x**4.0
    let quartic = square >> square

    ///cube root,立方根，x**(1.0/3.0)
    let cbrt x:float = x**(1.0/3.0)

    /// pi => 3.1415926...
    let pi = Math.PI

    ///给定数的自然对数
    let ln = log

    /// gravity，重力加速度,9.80665 m/s^2
    let g = 9.80665