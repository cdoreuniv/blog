#' A function dividing a triangle to create a  Sierpiński's triangle
#'
#' @param xA A point abcsissa  
#' @param yA A point ordinate
#' @param xB B point abcsissa
#' @param yB B point ordinate
#' @param xC C point abcsissa
#' @param yC C point ordinate
#'
#' @return a list of triangles
#' @export
#'
#' @examples  divide_triangle(0,0,3,3,6,0)
#' 
divide_triangle = function(xA,yA,xB,yB,xC,yC){
  triangle1 = constriangle(xA,yA,xB,yB,xC,yC)
  demi_triangle1 = demi_triangle(xA,yA,xB,yB,xC,yC)
  triangle2 = constriangle(xA, yA, demi_triangle1[1,1], demi_triangle1[1,2], demi_triangle1[3,1], demi_triangle1[3,2])
  triangle3 = constriangle( demi_triangle1[1,1], demi_triangle1[1,2], xB,yB, demi_triangle1[2,1], demi_triangle1[2,2])
  triangle4 = constriangle(demi_triangle1[3,1], demi_triangle1[3,2], demi_triangle1[2,1], demi_triangle1[2,2], xC,yC)
  triangle = list(triangle2, triangle3, triangle4)
  return(triangle)
}




#' A function dividing each triangle of a list of triangles to create a Sierpiński's triangle
#'
#' @param liste_triangles list of triangle on matrix form
#'
#' @return a list of triangle
#' @export
#'
#' @examples divide_list_triangle(divide_triangle(0,0,3,3,6,0))
divide_list_triangle = function(liste_triangles){
  n = length(liste_triangles)
  liste = vector(mode = "list", length = n)
  for(i in 1:n){
    trianglei = liste_triangles[[i]]
    liste[[i]] = divide_triangle(trianglei[1,1], #xA
                                 trianglei[1,2], #yA
                                 trianglei[2,1], #xB
                                 trianglei[2,2], #yB
                                 trianglei[3,1], #xC
                                 trianglei[3,2]  #yC
                                 )
  }
  return(liste)
}