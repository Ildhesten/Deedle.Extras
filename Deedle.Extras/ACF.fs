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

  /// <summary>
  /// Gets a series of auto correlations computed by the acf.
  /// </summary>
  /// <param name="acf">The result computed by acf.</param>
  let autoCorrelations acf =
    acf.AutoCorrelations

  /// <summary>
  /// Gets the significance level for autocorrelations on 
  /// the underlying time series.
  /// </summary>
  /// <param name="acf">The result computed by acf.</param>
  let bound acf =
    acf.Bound

  /// <summary>
  /// Computes estimates of the autocorrelation function, with the max.
  /// input lag specified.
  /// </summary>
  /// <param name="xs">The series to compute the autocorrelations for.</param>
  /// <param name="n">The maximum lag (value included).</param>
  let acfN (xs : Series<'a, float>) (n : int) =
    let computeAutoCorrelationFor (x : Series<'a,float>) (n : int) =
      let xs = x |> Series.skip n |> Series.values 
      let laggedXs = Series.shift n x |> Series.values
      Correlation.Pearson(xs, laggedXs)
    let autoCorrelations =
      Seq.init (n + 1) id
      |> Seq.map (computeAutoCorrelationFor xs)
      |> Series.ofValues
    let bound = 2.0 / sqrt (Series.countKeys xs |> float)
    {
      MaxLag = n
      AutoCorrelations = autoCorrelations
      Bound = bound
    }
  
  /// <summary>
  /// Computes estimates of the autocorrelation function.
  /// </summary>
  /// <param name="xs">The series to compute the autocorrelations for.</param>
  let acf (xs : Series<'a, float>) =
    let nObs = xs.KeyCount |> float
    let n = 
      10.0 * (log10 nObs) 
      |> floor 
      |> min (nObs - 1.0)
      |> int
    acfN xs n