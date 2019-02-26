namespace Deedle.Extras

module ACF =
  open Deedle

  type t

  val autoCorrelations : t -> Series<int, float>
  val bound : t -> float

  val acfN : Series<'a,float> -> int -> t
  val acf : Series<'a, float> -> t