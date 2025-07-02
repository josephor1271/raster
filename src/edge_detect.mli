open! Core

val transform
  :  Image.t
  -> blur_radius:int
  -> threshold_fraction:float
  -> Image.t

val command : Command.t
