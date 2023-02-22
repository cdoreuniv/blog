###################################################
########### CONSTRUCTION D'UN TRIANGLE ############
###################################################

#' Construction d'un triangle
#' Construction d'un triangle à partir des coordonnées de chaque point
#'
#' @param xA abcsisse de A
#' @param yA ordonnée de A
#' @param xB abcsisse de B
#' @param yB ordonnée de B
#' @param xC abcsisse de C
#' @param yC ordonnée de C
#'
#' @return un tableau avec les abcsisses sur la première colonne et les ordonnées sur la seconde
#' @export
#'
#' @examples constriangle(0,0,3,3,6,0)
#'
constriangle = function(xA,yA,xB,yB,xC,yC){
  xtriangle = c(xA,xB,xC) # abcsisses du triangle
  ytriangle = c(yA,yB,yC) # ordonnées du triangle
  triangle = cbind(xtriangle, ytriangle)
  return(triangle)
}


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

###################################################
############## DIVISION D'UN TRIANGLE #############
###################################################

#'
#' A function dividing a triangle to create a  Sierpiński's triangle
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
  for(i in liste_triangles){
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

###################################################
##### REPRÉSENTATION D'UNE LISTE DE TRIANGLES #####
###################################################

plot_triangles = function(list_triangles){
  df = data.frame()

  for (i in seq_along(list_triangles)) {
    dfi = data.frame(x = list_triangles[[i]][,1], y = list_triangles[[i]][,2])
    df = rbind(df, dfi)
  }

  g = ggplot(df, aes(x, y )) +
    geom_polygon(color = "black") +
    theme_void() +
    scale_fill_manual(values = rep("black", length(list_triangles)), guide = "none")

  return(g)
}


calcul_longueur = function(xA,yA,xB,yB){
  AB2 = (xB-xA)^2 + (yB - yA)^2
  AB = sqrt(AB2)
  return(AB)
}


calcul_aire = function(list_triangles){
  somme = 0
  for(i in seq_along(list_triangles)){
    AB = calcul_longueur( list_triangles[[i]][1,1], list_triangles[[i]][1,2],
                          list_triangles[[i]][2,1], list_triangles[[i]][2,2])
    BC = calcul_longueur( list_triangles[[i]][2,1], list_triangles[[i]][2,2],
                          list_triangles[[i]][3,1], list_triangles[[i]][3,2])
    AC = calcul_longueur( list_triangles[[i]][1,1], list_triangles[[i]][1,2],
                          list_triangles[[i]][3,1], list_triangles[[i]][3,2])
    aire = heron2(AB,BC,AC)
    somme = somme + aire
  }
  return(somme)
}




















