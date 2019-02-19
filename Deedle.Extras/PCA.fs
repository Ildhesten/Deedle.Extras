namespace Deedle.Extras

module PCA =
  open Deedle
  open MathNet.Numerics.LinearAlgebra
  open MathNet.Numerics.Statistics

  type t<'a> when 'a : equality =
    {
      PrincipalComponents : Frame<'a, string>
    }

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


  // TODO: Wrap this in the data type in this module so we can get both eigen values and vectors.  
  /// <summary>
  /// Computes the principal components from the data frame.
  /// The principal components are listed from PC1 .. PCn
  /// Where PC1 explains most of the variance.
  /// </summary>
  /// <param name="dataFrame">The data frame to do PCA</param>
  let pca dataFrame =
    let factorization = 
      dataFrame
      |> Frame.toArray2D
      |> M.DenseOfArray
      |> covarianceMatrixOf
      |> Matrix.eigen
    
    let eigenValues = 
      // eigen values are returned with least significant first.
      factorization.EigenValues
      |> Vector.map (fun x -> x.Real)
      |> Vector.toSeq
      |> Seq.rev
    let eigenVectors = 
      // as eigen vectors match the eigen values, these also has to be reversed.
      factorization.EigenVectors
      |> Matrix.toColSeq 
      |> Seq.rev 
      |> Matrix.Build.DenseOfColumnVectors
    
    if eigenVectors.RowCount <> dataFrame.ColumnCount then
      failwith "Row count of eigen vectors does not match the input columns"
    let colKeyArray = dataFrame.ColumnKeys |> Seq.toArray
    eigenVectors
    |> Matrix.toArray2
    |> Frame.ofArray2D
    |> Frame.mapColKeys (fun i -> sprintf "PC%d" (i + 1))
    |> Frame.mapRowKeys (fun i -> colKeyArray.[i])

    //let projector (observation:float[]) =
    //  let observationVector = observation |> Vector.Build.DenseOfArray
    //  (eigenVectors.Transpose() * observationVector)
    //  |> Vector.toArray
      
    //(eigenValues, eigenVectors, hest), projector