module Geometry
type point = int * int
type colour = int * int * int
type figure =
    | Circle of point * int * colour
    | Rectangle of point * point * colour
    | Mix of figure * figure
val colourAt : x:int * y:int -> figure:figure -> colour option
