namespace Deedle.Extras

module PCA =
  open Deedle
  open MathNet.Numerics.LinearAlgebra
  open MathNet.Numerics.Statistics

  type t<'a> when 'a : equality =
    {
      EigenVectors : Frame<'a, string>
      EigenValues : Series<string, float>
    }

  /// <summary>
  /// The eigen values of the PCA transformation.
  /// </summary>
  /// <param name="pca">The PCA transformation.</param>
  let eigenValues pca =
    pca.EigenValues

  /// <summary>
  /// The eigen vectors of the PCA transformation.
  /// </summary>
  /// <param name="pca">The PCA transformation.</param>
  let eigenVectors pca =
    pca.EigenVectors
  
  // TODO: Clean this mess up.
  let M = Matrix<float>.Build

  let correlationMatrixOf dataFrame =
    dataFrame
    |> Frame.toArray2D
    |> M.DenseOfArray
  
  let covarianceMatrixOf (M:Matrix<float>) =
    let noOfColumns = M.ColumnCount
    let C = DenseMatrix.create noOfColumns noOfColumns Matrix.Zero
    for c1 in 0 .. (noOfColumns - 1) do
      C.[c1,c1] <- Statistics.Variance (M.Column c1)
      for c2 in (c1 + 1) .. (noOfColumns - 1) do
        let covariance = Statistics.Covariance (M.Column c1, M.Column c2)
        C.[c1,c2] <- covariance
        C.[c2,c1] <- covariance
    C

  let normalize' noOfColumns (observations:float[][]) =
    let averages =
      Array.init noOfColumns (fun i -> 
        observations
        |> Seq.averageBy (fun x -> x.[i]))  
    observations
    |> Array.map (fun row -> Array.mapi (fun i x -> (x - averages.[i])) row)
                   
  let normalize noOfColumns (observations:float[][]) =
    let averages =
      Array.init noOfColumns (fun i -> 
        observations
        |> Seq.averageBy (fun x -> x.[i]))    
    let stdDevs =
      Array.init noOfColumns (fun i ->
        let average = averages.[i]
        observations
        |> Seq.averageBy (fun x -> pown (float x.[i] - average) 2 |> sqrt))    
    observations
    |> Array.map (fun row -> Array.mapi (fun i x -> (x - averages.[i]) / stdDevs.[i]) row)

  let normalizeSeriesUsing (mean : float) (stdDev : float) series =
    let normalizeValue x =
      (x - mean) / stdDev
    series
    |> Series.mapValues normalizeValue
    
  let normalizeSeries (series: Series<'a,float>) =
    let mean = Stats.mean series
    let stdDev = Stats.stdDev series
    normalizeSeriesUsing mean stdDev series

  let normalizeSeriesMedian (series: Series<'a,float>) =
    let normalizeValue median stdDev x =
      (x - median) / stdDev
    let median = Stats.median series
    let stdDev = Stats.stdDev series
    series
    |> Series.mapValues (normalizeValue median stdDev)

  let normalizeColumnsMedian (df:Frame<'a,'b>) =
    let normalizeColumn (k:'b) (row:ObjectSeries<'a>) =
      row.As<float>()
      |> normalizeSeriesMedian
    df
    |> Frame.mapCols normalizeColumn

  let normalizeColumnsUsing (mean : Series<'b, float>) (stdDev : Series<'b, float>) (df : Frame<'a, 'b>) =
    let normalizeColumn (k : 'b) (row : ObjectSeries<'a>) =
      row.As<float>()
      |> normalizeSeriesUsing (mean.[k]) (stdDev.[k])
    df
    |> Frame.mapCols normalizeColumn

  /// <summary>
  /// Normalizes the columns in the dataframe using a z-score.
  /// That is (X - mean) / (std. dev)
  /// </summary>
  /// <param name="df">The dataframe to normalize</param>
  let normalizeColumns (df:Frame<'a,'b>) =
    let normalizeColumn (k:'b) (row:ObjectSeries<'a>) =
      row.As<float>()
      |> normalizeSeries
    df
    |> Frame.mapCols normalizeColumn


  /// <summary>
  /// Computes the principal components from the data frame.
  /// The principal components are listed from PC1 .. PCn
  /// Where PC1 explains most of the variance.
  /// </summary>
  /// <param name="dataFrame">A PCA datatype that contains the eigen values and vectors.</param>
  let pca dataFrame =
    let factorization = 
      dataFrame
      |> Frame.toArray2D
      |> M.DenseOfArray
      |> covarianceMatrixOf
      |> Matrix.eigen
    
    let createPcNameForIndex n =
      sprintf "PC%d" (n + 1)

    let colKeyArray = dataFrame.ColumnKeys |> Seq.toArray    

    let eigenValues = 
      // eigen values are returned with least significant first.
      factorization.EigenValues
      |> Vector.map (fun x -> x.Real)
      |> Vector.toSeq
      |> Seq.rev
      |> Series.ofValues
      |> Series.mapKeys createPcNameForIndex
    let eigenVectors = 
      // as eigen vectors match the eigen values, these also has to be reversed.
      factorization.EigenVectors
      |> Matrix.toColSeq 
      |> Seq.rev 
      |> Seq.mapi (fun i x -> (createPcNameForIndex i, Vector.toSeq x |> Series.ofValues))
      |> Frame.ofColumns
      |> Frame.mapRowKeys (fun i -> colKeyArray.[i])
    
    if eigenVectors.RowCount <> dataFrame.ColumnCount then
      failwith "Row count of eigen vectors does not match the input columns"    
    {
      EigenValues = eigenValues
      EigenVectors = eigenVectors
    }