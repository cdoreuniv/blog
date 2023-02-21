constriangle = function(xA,yA,xB,yB,xC,yC){
  #xtriangle = vector(mode = "list", length = 3)
  xtriangle = c(xA,xB,xC)
  #ytriangle = vector(mode = "list", length = 3)
  ytriangle = c(yA,yB,yC)
  triangle = cbind(xtriangle, ytriangle)
  return(triangle)
}