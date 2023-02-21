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
  xAB = A[[1]] + B[[1]]
  xAC = A[[1]] + C[[1]]
  xBC = B[[1]] + C[[1]]
  yAB = A[[2]] + B[[2]]
  yAC = A[[2]] + C[[2]]
  yBC = B[[2]] + C[[2]]
  
  xAprime = xAB/2
  xBprime = xBC/2
  xCprime = xAC/2
  yAprime = yAB/2
  yBprime = yBC/2
  yCprime = yAC/2

  xABC = c(A[[1]],B[[1]],C[[1]])
  yABC = c(A[[2]],B[[2]],C[[2]])
  
  xAAprimeCprime = c(A[[1]], xAprime, xCprime)
  yAAprimeCprime = c(A[[2]], yAprime, yCprime)
  
  xAprimeBBprime = c(xAprime, B[[1]], xBprime)
  yAprimeBBprime = c(yAprime, B[[2]], yBprime)
  
  xCprimeBprimeC = c(xCprime, xBprime, C[[1]])
  yCprimeBprimeC = c(yCprime, yBprime, C[[2]])
  
  triangle1 = list(xABC, yABC)
  triangle2 = list(xAAprimeCprime, yAAprimeCprime)
  triangle3 = list(xAprimeBBprime, yAprimeBBprime)
  triangle4 = list(xCprimeBprimeC, yCprimeBprimeC)

  return(list(triangle1, triangle2, triangle3, triangle4))
}  





#' Division de triangles
#'
#' @param liste liste de triangles
#'
#' @return une liste de triangles
#' @export
#'
#' @examples
divide_list_triangle = function(liste){
  listetri = c()
  for(i in 1:length(liste)){
    A = c(liste[[i]][[1]][[1]], liste[[i]][[2]][[1]])
    B = c(liste[[i]][[1]][[2]], liste[[i]][[2]][[2]])
    C = c(liste[[i]][[1]][[3]], liste[[i]][[2]][[3]])
    
    listetri = c(listetri, divide_triangle(A,B,C))
  }
  return(listetri)
}









