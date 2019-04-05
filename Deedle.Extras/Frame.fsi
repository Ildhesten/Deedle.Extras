namespace Deedle.Extras

module Frame =
  open Deedle

  val filterRowsByKey : ('a -> bool) -> Frame<'a,'b> -> Frame<'a,'b>