namespace Deedle.Extras

module LinearRegression =
  open Deedle

  module Fit =
    type t<'a,'b> when 'a : equality and 'b : equality

    val input : t<'a,'b> -> Frame<'a,'b>
    val coefficients : t<'a,'b> -> Series<'b, float>
    val fitIntercept : t<'a,'b> -> 'b option
    val fittedValues : t<'a,'b> -> Series<'a, float>
    val residuals : t<'a,'b> -> Series<'a,float>

    module Summary =
      type t<'b> when 'b : equality =
        {
          LinearFormula : string
          ResidualFiveVals : Series<string,float>
          TTable : Frame<'b,string>
          RSquared : float
          AdjRSquared : float
        }      

    val summary : t<'a,'b> -> Summary.t<'b> when 'b : comparison
  
  val multiDim : 'b seq -> 'b -> 'b option -> Frame<'a,'b> -> Fit.t<'a,'b> when 'b : comparison
  val simple : 'b -> 'b -> 'b option -> Frame<'a,'b> -> Fit.t<'a,'b> when 'b : comparison