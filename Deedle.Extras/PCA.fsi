namespace Deedle.Extras

module PCA = 
  open Deedle
  type t<'a> when 'a : equality

  val normalizeColumns : Frame<'a,'b> -> Frame<'a,'b>
  val pca : Frame<'a,'b> -> Frame<'b,string>