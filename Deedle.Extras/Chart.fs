namespace Deedle.Extras.Charting
open Deedle
open XPlot.Plotly

type Chart() =
  static member private LineGraph (s : Series<'a,'b>, name : string option) =
    let observations = Series.observations s
    let xs = Seq.map fst observations
    let ys = Seq.map snd observations
    match name with
    | None -> Graph.Scatter(x = xs, y = ys, mode = "Line")
    | Some name -> Graph.Scatter(x = xs, y = ys, mode = "Line", name = name)
    
  static member private ScatterGraph (s : Series<'a,'b>, name : string option) =
    let observations = Series.observations s
    let xs = Seq.map fst observations
    let ys = Seq.map snd observations
    match name with
    | None -> Graph.Scatter(x = xs, y = ys, mode="markers")
    | Some name -> Graph.Scatter(x = xs, y = ys, mode = "markers", name = name)

  static member Line (s : Series<'a,'b>, ?name : string) =
    Chart.LineGraph (s,name)
    |> Chart.Plot

  static member Line (s : Series<'a,'b> seq, names : string option seq) =
    if Seq.length s <> Seq.length names then
      failwith "Sequence of data series must be of same length as the names"
    let namedSeries = Seq.zip s names
    namedSeries
    |> Seq.map Chart.LineGraph
    |> Seq.map (fun x -> x :> Graph.Trace)
    |> Chart.Plot

  static member Line (s : Series<'a,'b> seq, names : string seq) =
    let mappedNames = Seq.map Some names
    Chart.Line (s, mappedNames)

  static member Scatter (s : Series<'a,'b> seq, names : string option seq) =
    if Seq.length s <> Seq.length names then
      failwith "Sequence of data series must of same length as the names"
    let namedSeries = Seq.zip s names
    namedSeries
    |> Seq.map Chart.ScatterGraph
    |> Seq.map (fun x -> x :> Graph.Trace)
    |> Chart.Plot

  static member Scatter (s : Series<'a,'b> seq, names : string seq) =
    let mappedNames = Seq.map Some names
    Chart.Scatter (s, mappedNames)

/// <summary>
/// This module contains helper methods for plotting data from data frames.
/// </summary>
type Frame() =

  /// <summary>
  /// Plots all columns in the data frame as line plots.
  /// specified in the in the column key sequence.
  /// </summary>
  /// <param name="df">The data frame to plot.</param>
  /// <param name="columnKeys">The column keys you want to plot.</param>
  static member LineColumnKeys(df : Frame<'a,'b>, columnKeys : 'b seq) =
    let names = 
      columnKeys 
      |> Seq.map (sprintf "%A") 
      |> Seq.map Some
    let series = 
      columnKeys 
      |> Seq.map (fun k -> df.[k])
    Chart.Line(series, names)
  
  /// <summary>
  /// Plots all columns in the data frame as scatter plots.
  /// specified in the in the column key sequence.
  /// </summary>
  /// <param name="df">The data frame to plot.</param>
  /// <param name="columnKeys">The column keys you want to plot.</param>
  static member ScatterColumnKeys(df : Frame<'a,'b>, columnKeys : 'b seq) =
    let names = 
      columnKeys 
      |> Seq.map (sprintf "%A") 
      |> Seq.map Some
    let series = 
      columnKeys 
      |> Seq.map (fun k -> df.[k])
    Chart.Scatter(series, names)    

  /// <summary>
  /// Plots all columns in the data frame as scatter plots.
  /// </summary>
  /// <param name="df">The data frame to plot.</param>
  static member Scatter (df : Frame<'a,'b>) =
    Frame.ScatterColumnKeys(df, df.ColumnKeys)

  /// <summary>
  /// Plots all columns in the data frame as line plots.
  /// </summary>
  /// <param name="df">The data frame to plot.</param>
  static member Line (df : Frame<'a,'b>) =
    Frame.LineColumnKeys (df, df.ColumnKeys)

module LinearRegression =
  open Deedle.Extras.LinearRegression.Fit
  type Fit () =
    
    /// <summary>
    /// Produces a 2D plot of a "slice" of the linear regression. If this is a multiple linear regression
    /// the lines is fitted using the coefficient of the provided key and the intercept from the regression.
    /// Therefore the fit may seem worse than if a simple regression had been applied.
    /// </summary>
    /// <param name="xKey">The key to plot the slice for.</param>
    /// <param name="fit">The fit to plot the slice from.</param>
    static member Plot (xKey : 'b, fit : t<'a,'b>) =
      let xCoefficient = (coefficients fit).[xKey]
      let intercept =
        match fitIntercept fit with
        | None -> 0.0
        | Some interceptKey -> (coefficients fit).[interceptKey]
      let yKey = yKey fit
      let df = input fit
      let xVals = df.[xKey] |> Series.values
      let yVals = df.[yKey] |> Series.values      
      let fittedData = xVals |> Seq.map (fun x -> x * xCoefficient + intercept)
      [
        Graph.Scatter (x = xVals, y = yVals, mode="markers", name = "Scatter plot");
        Graph.Scatter (x = xVals, y = fittedData, mode="line", name = "Fit")
      ] 
      |> Chart.Plot
      |> Chart.WithXTitle (xKey.ToString ())
      |> Chart.WithYTitle (yKey.ToString ())

module ACF =
  open Deedle.Extras.ACF

  /// <summary>
  /// Plots the autocorrelations from an acf.
  /// The bars are autocorrelations of different lags, and the 
  /// dashed lines are the significance levels.
  /// </summary>
  /// <param name="acf">The acf to plot</param>
  let Plot (acf : t) =
    let acs = autoCorrelations acf
    let bound = bound acf
    let noOfAcs = acs |> Series.countKeys
    let upperBound = Seq.init noOfAcs (fun _ -> bound)
    let lowerBound = Seq.init noOfAcs (fun _ -> -bound)
    [
      XPlot.Plotly.Graph.Scatter (x = acs.Keys, y = lowerBound, mode = "line", line = new Line(color = "cornflowerblue", dash = "4")) :> Trace;
      XPlot.Plotly.Graph.Bar (x = acs.Keys, y = acs.Values) :> Trace;
      XPlot.Plotly.Graph.Scatter (x = acs.Keys, y = upperBound, mode = "line", line = new Line(color = "cornflowerblue", dash = "4")) :> Trace
    ]
    |> Chart.Plot
    |> Chart.WithLabels ["Significance limit"; "Autocorrelations"; "Significance limit"]
    |> Chart.WithXTitle "Lag"
    |> Chart.WithYTitle "Correlation"