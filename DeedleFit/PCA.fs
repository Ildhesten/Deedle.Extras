namespace DeedleFit

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
    
  let normalizeColumns (df:Frame<'a,'b>) =
    let means = Stats.mean df
    let stdDevs = Stats.stdDev df
    let normalizeValue mean stdDev x =
      (x - mean) / stdDev
    let normalizeColumn (k:'b) (row:ObjectSeries<'a>) =
      let mean = means.[k]
      let stdDev = stdDevs.[k]
      row.As<float>()
      |> Series.mapValues (normalizeValue mean stdDev)
    df
    |> Frame.mapCols normalizeColumn


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
      |> Vector.toSeq
      |> Seq.map (fun x -> x.Real)
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