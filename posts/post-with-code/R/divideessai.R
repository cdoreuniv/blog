#' Tracé du triangle de Sierpinski
#'
#' @param A coordonnées de A sous forme de liste
#' @param B coordonnées de B sous forme de liste
#' @param C coordonnées de C sous forme de liste
#'
#' @return a plot 
#' @export
#'
#' @examples divide_triangle(A,B,C)
#' 
divide_triangle = function(A,B,C){
  x = c(A[[1]],B[[1]],C[[1]])
  y = c(A[[2]],B[[2]],C[[2]])
  
  xAB = A[[1]] + B[[1]]
  xAC = A[[1]] + C[[1]]
  xBC = B[[1]] + C[[1]]
  
  yAB = A[[2]] + B[[2]]
  yAC = A[[2]] + C[[2]]
  yBC = B[[2]] + C[[2]]
  
  demxAB = xAB/2
  demxBC = xBC/2
  demxAC = xAC/2
  demyAB = yAB/2
  demyBC = yBC/2
  demyAC = yAC/2
  
  demx = c(demxAB,demxAC,demxBC, demxAB)
  demy = c(demyAB, demyAC, demyBC, demyAB)
  
  tr1 = ggplot() + 
    geom_line(aes(x,y), color = "blue") +
    geom_line(aes(c(C[[1]],A[[1]]), c(C[[2]],A[[2]])), color = "blue")
  
  sp1 = tr1 +
  geom_line(aes(demx, demy)) +
  geom_line(aes( c(demxAB,demxBC), c(demyAB, demyBC)  )) +
  title("Triangle de Sierpinski")
  
  return(sp1)
}  
