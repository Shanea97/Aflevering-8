open ImgUtil
/// Opgave 8i0
let pic = ImgUtil.mk 141 141

let circle = Circle((50,50), (45), (0,0,255))
let mix = Mix ((Rectangle((90,110), (40,40), (255,0,0))), (Circle((50,50), (45), (0,0,255))))
let square = Rectangle((90,110), (40,40), (255,0,0))

let round bmp fig =
  for x = 0 to 140 do /// Does a for-loop through the BitMap x-cordinates
    for y = 0 to 140 do // Does a for-loop through the BitMap y-cordinates
      match (colourAt (x,y) fig) with /// Checks to see if the figure is inside the cordinates, and prints a colour if that is the case
      | Some col -> ImgUtil.setPixel(ImgUtil.fromRgb ((colourAt (x,y) fig).Value)) (x,y) pic // prints a colour
      | None  -> ImgUtil.setPixel(ImgUtil.fromRgb (100,200,150)) (x,y) pic /// Prints a different colour for the
  do ImgUtil.toPngFile "figTest" pic

round pic mix

/// Opgave 8i1

//let circle = Circle((125,125), (125), (0,0,255))
/// <param name "filnavn" "figur" "b" and "h">
/// filnavn: string in form of the name of the given file you want to produce
/// figur: Figure
/// b: integer in form of width
/// h: integer in form of height
/// </param name>
/// <summary>
/// the functions makes a BitMap (bmp) that takes the form of (b x h), afterwards, by 2 for-loops, it runs through the whole BitMap and checks "colourAt" if one of the given figures is presented, and if this is true, it "prints" a colour onto the BitMap
/// </summary>
/// <returns>
/// The function returns a file, given the name of the String, which is a .png file, and you´re able to open your picture
/// </returns>
let MakePicture (filnavn:string) (figur:figure) (b:int) (h:int) =
  let bmp = ImgUtil.mk b h
  for x = 0 to (b-1) do
    for y = 0 to (h-1) do
      match (colourAt (x,y) figur) with
      | Some col -> ImgUtil.setPixel(ImgUtil.fromRgb ((colourAt (x,y) figur).Value)) (x,y) bmp
      | None  -> ImgUtil.setPixel(ImgUtil.fromRgb (125,125,125)) (x,y) bmp
  do ImgUtil.toPngFile filnavn bmp

MakePicture "8i1 billede.png" circle 250 250

// Opgave 8i2
/// Here the function MakePicture gets used to make a picture, using the figure made earlier "mix" that combines the
/// the circle and rectangle in one figure.
/// After this it gets given the size of the bitmap 100x150, and makes the file called figTest.png
MakePicture "figTest.png" mix 100 150

// Opgave 8i3

/// <param name (r, g, b)>
/// The parameter is a tripple consisting of the colors red green and blue
/// </param name>
/// <summary>
/// The function checks for each of the colors to be between 0 and 255
/// </summary>
/// <returns>
/// The function returns a boolean
/// </returns>
let CheckColour (r, g, b) : bool =
  r>=0 && r<=255 && g>=0 && g<=255 && b>=0 && b<=255

/// <param name "f">
/// The parameter f takes form of a figure
/// </param name>
/// <summary>
/// the function matches the parameter f with one of the three figures. and depending on which type it matches, it checks for different things, regarding of the figure turns out to be true. Here it also uses the function CheckColour
/// </summary>
/// <returns>
/// The function returns a boolean
/// </returns>
let rec CheckFigure (f:figure): bool =
  match f with
  | Circle ((cx,cy),r,col) ->
      r > 0 && CheckColour col
  | Rectangle ((x0,y0), (x1,y1), col) ->
      x0 > x1 && y0 > y1 && CheckColour col
  | Mix (f1,f2) ->
      CheckFigure(f1) && CheckFigure(f2)
printfn "8i3\n"
printfn "Here the checkFigure and checkColour gets tested, to see if they does as calculated."
let mixcc = Mix(circle,circle)
let mixrr = Mix(square,square)
printfn "Circle: %b" (CheckFigure (Circle((125,125), (125), (0,0,255))))
printfn "Rectangle: %b" (CheckFigure (Rectangle((81,99), (36,36), (255,0,0))))
printfn "Mix between a circle and a rectangle: %b" (CheckFigure (mix))
printfn "Mix between two circles: %b" (CheckFigure (mixcc))
printfn "Mix between two rectangles: %b\n" (CheckFigure (mixrr))

// 8i4

/// <param name "fig" "(z,c)">
/// "fig" takes form of the type Figure
/// (z, c) is a tupple, of which you want to move the figure (fig)
/// </param name>
/// <summary>
/// The function takes a figure, of any of the 3 sorts that have been given, an is able to move the figure around, compared to where it originally would have been
/// </summary>
/// <returns>
/// The function returns another figure
/// </returns>
let rec move (fig:figure) (z, c) : figure =
  match fig with
  | Circle ((cx,cy),r,col) ->
      Circle ((cx+z,cy+c),r,col)
  | Rectangle ((x0,y0), (x1,y1), col) ->
      Rectangle ((x0+z,y0+c), (x1+z,y1+c), col)
  | Mix (f1,f2) ->
      Mix(move (f1) (z,c),move(f2) (z,c))

let figTest = Mix (circle, square)
MakePicture "moveTest.png" (move figTest (-20,20)) 100 150


// 8i5
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
     ((fst(boundingBox(f1)),(snd(boundingBox(f2)))))

let square5 = Rectangle((40,40), (90,110), (255,0,0))

let circle5 = Circle((50,50), (45), (0,0,255))
let mix5 = Mix ((circle,square))
/// Jeg har en god ide om at det første par i tupplen skal være så lille som mulig og det andet par skal være så stort som muligt. Men jeg har svært ved at finde en "nem" måde at gøre det på, hvor man ikke nødvendigvis skal ud i flere linjer lang "If, Then, Else" kommandoer. jeg kan ikke lige lurer hvordan jeg skal implementere det.

printfn "8i5\n"
printfn "Square:%A" (boundingBox square5)
printfn "Circle:%A" (boundingBox circle5)
printfn "Mix:%A" (boundingBox mix5)
