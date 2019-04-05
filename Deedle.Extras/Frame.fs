namespace Deedle.Extras

module Frame =
  open Deedle

  /// <summary>
  /// Filters the rows in the frame, if the row key matches the predicate.
  /// </summary>
  /// <param name="predicate">The predicate to filter the row keys.</param>
  /// <param name="frame">The frame to apply the predicate to.</param>
  let filterRowsByKey predicate frame =
    Frame.filterRows (fun key _ -> predicate key) frame

