namespace DeedleFit.Charting
open Deedle
open XPlot.Plotly
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

  static member Line(df : Frame<'a,'b>, columnKeys : 'b seq) =
    let names = columnKeys |> Seq.map (sprintf "%A") |> Seq.map Some
    let series = columnKeys |> Seq.map (fun k -> df.[k])
    Chart.Line(series, names)

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

  static member Scatter (df : Frame<'a,'b>, columnKeys : 'b seq) =
    let names = columnKeys |> Seq.map (sprintf "%A") |> Seq.map Some
    let series = columnKeys |> Seq.map (fun k -> df.[k])
    Chart.Scatter(series, names)