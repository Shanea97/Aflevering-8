open ImgUtil

let pic = ImgUtil.mk 141 141

let circle = Circle((50,50), (45), (0,0,255))
let mix = Mix ((Rectangle((40,40), (90,110), (255,0,0))), (Circle((50,50), (45), (0,0,255))))
let square = Rectangle((40,40), (90,110), (255,0,0))

let round bmp fig =
  for x = 0 to 140 do
    for y = 0 to 140 do
      match (colourAt (x,y) fig) with
      | Some col -> ImgUtil.setPixel(ImgUtil.fromRgb ((colourAt (x,y) fig).Value)) (x,y) pic
      | None  -> ImgUtil.setPixel(ImgUtil.fromRgb (100,200,150)) (x,y) pic

do ImgUtil.toPngFile "figTest" pic
