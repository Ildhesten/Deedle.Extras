namespace Deedle.Extras

module PCA = 
  open Deedle
  type t<'a> when 'a : equality

  val eigenValues : t<'a> -> Series<string, float>
  val eigenVectors : t<'a> -> Frame<'a,string>

  val normalizeColumns : Frame<'a,'b> -> Frame<'a,'b>
  val pca : Frame<'a,'b> -> t<'b>