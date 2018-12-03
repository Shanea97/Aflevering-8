open ImgUtil 
let rec boundingBox (fig:figure) : point*point =
  match fig with
  | Circle ((cx,cy), r, col) ->
      ((cx-r,cy-r),(cx+r,cy+r))
  | Rectangle ((x0,y0), (x1,y1), col) ->
      ((x0,y0), (x1,y1))
  | Mix (f1,f2) ->
     (((min(fst(fst(f1)))(fst(fst(f2)))),min(snd(fst(f1))(snd(fst(f2))))),((max(fst(snd(f1)))(fst(snd(f2)))),max(snd(fst(f1)))(snd(snd(f2)))))

let square = Rectangle((40,40), (90,110), (255,0,0))

let circle = Circle((50,50), (45), (0,0,255))
let mix = Mix ((circle,square))