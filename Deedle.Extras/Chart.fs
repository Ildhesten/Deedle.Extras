namespace Deedle.Extras.Charting
open Deedle
open XPlot.Plotly

type Chart() =
  static member private LineGraph (s : Series<'a,'b>, name : string option) =
    match name with
    | None -> Graph.Scatter(x = s.Keys, y = s.Values, mode = "Line")
    | Some name -> Graph.Scatter(x = s.Keys, y = s.Values, mode = "Line", name = name)
    
  static member private ScatterGraph (s : Series<'a,'b>, name : string option) =
    match name with
    | None -> Graph.Scatter(x = s.Keys, y = s.Values, mode="markers")
    | Some name -> Graph.Scatter(x = s.Keys, y = s.Values, mode = "markers", name = name)

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