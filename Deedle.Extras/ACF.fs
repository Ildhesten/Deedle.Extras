namespace Deedle.Extras

module ACF =
  open Deedle
  open MathNet.Numerics.Statistics

  type t =
    {
      MaxLag : int
      AutoCorrelations : Series<int, float>
      Bound : float
    }

  let autoCorrelations acf =
    acf.AutoCorrelations

  let bound acf =
    acf.Bound

  let acfN (x : Series<'a, float>) (n : int) =
    let computeAutoCorrelationFor (x : Series<'a,float>) (n : int) =
      let xs = x |> Series.skip n |> Series.values 
      let laggedXs = Series.shift n x |> Series.values
      Correlation.Pearson(xs, laggedXs)
    let autoCorrelations =
      Seq.init (n + 1) id
      |> Seq.map (computeAutoCorrelationFor x)
      |> Series.ofValues
    let bound = 2.0 / sqrt (Series.countKeys x |> float)
    {
      MaxLag = n
      AutoCorrelations = autoCorrelations
      Bound = bound
    }

  let acf (x : Series<'a, float>) =
    let nObs = x.KeyCount |> float
    let n = 
      10.0 * (log10 nObs) 
      |> floor 
      |> min (nObs - 1.0)
      |> int
    acfN x n