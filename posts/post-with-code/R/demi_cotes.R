demi_cotes = function(xA,yA,xB,yB){
  x = xA + xB
  demix = x / 2
  y = yA + yB
  demiy = y / 2
  cote = cbind(demix, demiy)
  return(cote)
}

demi_triangle = function(xA,yA,xB,yB,xC,yC){
  cote = rbind(demi_cotes(xA,yA,xB,yB), demi_cotes(xB,yB,xC,yC), demi_cotes(xA,yA,xC,yC))
  return(cote)
}