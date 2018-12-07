let circle = ((50,35),(90,95))
let square = ((49,45),(10,120))
printfn "square: %A\ncircle: %A" square circle

let newTupple = (fst (fst circle), fst (fst square))
printfn "NewTupple: %A" newTupple
let fstMin = min  (fst (fst circle)) (fst (fst(square)))
let sndMin = min  (snd (fst circle)) (snd(fst(square)))
let NewMinTupple = (fstMin,sndMin)
let fstMax = max (fst(snd circle)) (fst (snd(square)))
let sndMax = max (snd(snd circle)) (snd (snd(square)))
let NewMaxTupple = (fstMax,sndMax)
let BoundingboxTupple = (NewMinTupple, NewMaxTupple)

printfn "fstMin: %A\nsndMin: %A\nNewMinTupple: %A" fstMin sndMin NewMinTupple
printfn "fstMax: %A\nsndMax: %A\nNewMaxTupple: %A" fstMax sndMax NewMaxTupple
printfn "BoundingboxTupple: %A" BoundingboxTupple