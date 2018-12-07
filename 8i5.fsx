open ImgUtil
/// <param name "fig">
/// Fig takes the form of figure
/// </param name>
/// <summary>
/// the function matches fig with one of the three figures, and corrosponds to that
/// by doing one of three things, and thereby checking for the smallest box, that covers the figure
/// </summary>
/// <returns>
/// A tupple of Points
/// </returns>
let rec boundingBox (fig:figure) : point*point =
  match fig with
  | Circle ((cx,cy), r, col) ->
      ((cx-r,cy-r),(cx+r,cy+r))
  | Rectangle ((x0,y0), (x1,y1), col) ->
      ((x0,y0), (x1,y1))
  | Mix (f1,f2) ->
      let ((x1,y1),(x2,y2)) = boundingBox f1
      let ((z1,c1),(z2,c2)) = boundingBox f2 
      let fstMin = min x1 z1
      let sndMin = min y1 c1 
      let fstMax = max x2 z2
      let sndMax = max y2 c2
      ((fstMin,sndMin),(fstMax,sndMax))

let square = Rectangle((40,40), (90,110), (255,0,0))

let circle = Circle((50,50), (45), (0,0,255))
let mix = Mix ((circle,square))


printfn "Square:%A" (boundingBox square)
printfn "Circle:%A" (boundingBox circle)
printfn "Mix:%A" (boundingBox mix) 
/// Jeg har en god ide om at det første par i tupplen skal være så lille som mulig og det andet par skal være så stort som muligt. Men jeg har svært ved at finde en "nem" måde at gøre det på, hvor man ikke nødvendigvis skal ud i flere linjer lang "If, Then, Else" kommandoer. jeg kan ikke lige lurer hvordan jeg skal implementere det.
