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
  triangle = c(xA,yA,xB,yB,xC,yC)
  return(triangle)
}


###################################################
####### CALCUL MOITIÉS CÔTÉS D'UN TRIANGLE ########
###################################################

demi_cotes = function(xA,yA,xB,yB){
  x = xA + xB
  demix = x / 2
  y = yA + yB
  demiy = y / 2
  cote = cbind(demix, demiy)
  return(cote)
}

demi_triangle = function(xA,yA,xB,yB,xC,yC){
  demi_ab = demi_cotes(xA,yA,xB,yB) # Point AB'
  demi_bc =  demi_cotes(xB,yB,xC,yC) # Point BC'
  demi_ac = demi_cotes(xA,yA,xC,yC) # Point AC'
  cote = c(demi_ab,demi_bc,demi_ac)
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
divide_triangle = function(coord =  c(xA,yA,xB,yB,xC,yC)){ # Merci Benoît-Alexandre pour l'idée de coord
  demi_triangle1 = demi_triangle(coord[1],coord[2],coord[3],coord[4],coord[5],coord[6])
  #  triangle1 = constriangle(coord[1],coord[2],coord[3],coord[4],coord[5], coord[6]) # Triangle A-B-C
  triangle2 = constriangle(coord[1],coord[2], demi_triangle1[1], demi_triangle1[2], demi_triangle1[5], demi_triangle1[6] ) # Triangle A-AB'-AC'
  triangle3 = constriangle(demi_triangle1[1], demi_triangle1[2], coord[3],coord[4], demi_triangle1[3], demi_triangle1[4] ) # Triangle AB'-B-BC'
  triangle4 = constriangle(demi_triangle1[5], demi_triangle1[6], demi_triangle1[3], demi_triangle1[4], coord[5], coord[6]) # Triangle AC'-BC'-C
  liste_triangles = list(triangle2, triangle3, triangle4)
  return(liste_triangles)
}


###################################################
####### DIVISION DES TRIANGLES D'UNE LISTE ########
###################################################


#' A function dividing each triangle of a list of triangles to create a Sierpiński's triangle
#'
#' @param liste_triangles list of triangle on matrix form
#'
#' @return a list of triangle
#' @export
#'
#' @examples divide_list_triangle(divide_triangle(0,0,3,3,6,0))
divide_list_triangle = function(liste_triangles){
  liste = list()
  for(x in liste_triangles){
    liste = append(liste,divide_triangle(x))
  }
  return(liste)
}


###################################################
##### REPRÉSENTATION D'UNE LISTE DE TRIANGLES #####
###################################################
library(ggplot2)
plot_triangles = function(liste_triangles){
  df = data.frame()
  for (i in seq_along(liste_triangles)) {
    dfi = data.frame(x = c(liste_triangles[[i]][1], liste_triangles[[i]][3], liste_triangles[[i]][5]),
                     y = c(liste_triangles[[i]][2], liste_triangles[[i]][4], liste_triangles[[i]][6]),
                     tri = i)
    df = rbind(df, dfi)
  }

  g1 = ggplot(df, aes(x, y ,fill = factor(tri))) +
    geom_polygon(color = "black") +
    theme_void()+
    scale_fill_manual(values = rep("black", length(liste_triangles)), guide = "none")


  return(g1)
}

###################################################
### CALCUL DE L'AIRE DES TRIANGLES D'UNE LISTE ####
###################################################

calcul_longueur = function(xA,yA,xB,yB){
  AB2 = (xB-xA)^2 + (yB - yA)^2
  AB = sqrt(AB2)
  return(AB)
}


calcul_aire = function(list_triangles){
  somme = 0
  for(i in seq_along(list_triangles)){
    AB = calcul_longueur( list_triangles[[i]][1], list_triangles[[i]][2],
                          list_triangles[[i]][3], list_triangles[[i]][4])
    BC = calcul_longueur( list_triangles[[i]][3], list_triangles[[i]][4],
                          list_triangles[[i]][5], list_triangles[[i]][6])
    AC = calcul_longueur( list_triangles[[i]][1], list_triangles[[i]][2],
                          list_triangles[[i]][5], list_triangles[[i]][6])
    aire = heron2(AB,BC,AC)
    somme = somme + aire
  }
  return(somme)
}

