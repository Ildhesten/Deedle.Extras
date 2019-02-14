namespace Deedle.Extras

module Stats =
  open MathNet.Numerics
  open Deedle

  let curriedCovariance (a : float[]) (b : float[]) =
    Statistics.ArrayStatistics.Covariance (a,b)

  let means (df : Frame<'a,'b>) : Series<'b, float> =
    let columnKeys = df.ColumnKeys
    columnKeys
    |> Seq.map (fun k -> df.[k] |> Stats.mean)
    |> Seq.zip columnKeys
    |> Series.ofObservations

  let inverse (df : Frame<'a, 'b>) =
    let arr = df |> Frame.toArray2D
    let matrix = LinearAlgebra.DenseMatrix.ofArray2 arr
    let inverseMatrix = matrix.Inverse()
    inverseMatrix.ToArray()
    |> Frame.ofArray2D
    |> Frame.indexColsWith df.ColumnKeys
    |> Frame.indexRowsWith df.RowKeys


  let covariance (df: Frame<'a,'b>) : Frame<'b,'b> =
    let columnKeys = df.ColumnKeys    
    let covarianceForColumnKey k =
      columnKeys
      |> Seq.map (fun k' ->
                    let a = df.[k'] |> Series.values|> Seq.toArray
                    let b = df.[k] |> Series.values |> Seq.toArray
                    if Array.length a <> Array.length b then
                      failwith "Vectors for correlation does not have same length"
                    Statistics.ArrayStatistics.Covariance(a,b)
                    )
      |> Seq.zip columnKeys
      |> Series.ofObservations
    let covariancesForEachColumnKey =
      columnKeys
      |> Seq.map covarianceForColumnKey
      |> Seq.zip columnKeys
    Frame.ofRows covariancesForEachColumnKey

