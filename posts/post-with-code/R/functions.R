##Fonction : centre_segment##

#' Fonction permettant de calculer le centre d'un segment
#'
#' @param x_a Coordonnée x d'un premier point
#' @param y_a Coordonnée y d'un premier point
#' @param x_b Coordonnée x d'un e uxième point
#' @param y_b Coordonnée y d'un deuxième point
#'
#' @return Les coordonnées du centre d'un segment
#' @export
#'
#' @examples
#' centre_segment(1,2,3,1)
centre_segment <-function(x_a,y_a,x_b,y_b){ 
  return(c((x_a+x_b)/2,(y_a+y_b)/2))
}


##Fonction : divide_triangle##

#' Fonction permettant de diviser un triangle grâce à l'algorithme de Sierpinski
#'
#' @param vect_coord cordonnées du triangle
#'
#' @return une liste de corrdonnées des points des triangles divisés
#' @export
#'
#' @examples
#' divide_triangle(c(0,0,0,1,0.5,sqrt(3)/2))
divide_triangle <- function(vect_coord = c(x_a, y_a, x_b, y_b, x_c, y_c)) {
    # Coordonnées du milieu des segments
    c_ab <- centre_segment(vect_coord[1], vect_coord[2], vect_coord[3], vect_coord[4])
    c_ac <- centre_segment(vect_coord[1], vect_coord[2], vect_coord[5], vect_coord[6])
    c_bc <- centre_segment(vect_coord[3], vect_coord[4], vect_coord[5], vect_coord[6])
    # Création des nouveaux triangles
    t_1 <- c(vect_coord[1], vect_coord[2], c_ab[1], c_ab[2], c_ac[1], c_ac[2])
    t_2 <- c(vect_coord[3], vect_coord[4], c_ab[1], c_ab[2], c_bc[1], c_bc[2])
    t_3 <- c(vect_coord[5], vect_coord[6], c_ac[1], c_ac[2], c_bc[1], c_bc[2])
    return(list(t_1, t_2, t_3))
}


##Fonction : didive_list_triangle##

#' Fonction permettant de prendre une liste de triangle en renvoyant une liste
#'
#' @param list_triangles une liste de triangles
#'
#' @return une autre liste de triangles
#' @export
#'
#' @examples 
#' divide_list_triangle(divide_triangle(vect_coord = c(0,1,0,0,3,4)))
divide_list_triangle = function(list_triangles){
  liste = list()
  for(i in list_triangles){
    liste = append(liste,divide_triangle(i))
  }
  return(liste)
}


##Fonction : plot_triangles##

#' Fonction permettant de tracer des triangles étant rentrés comme une liste
#'
#' @param list_triangles une liste de triangle
#'
#' @return Graphique des triangles
#' @import ggplot2
#' @export
#'
#' @examples
#' Iteration_1 <- divide_triangle(triangle)
#' plot_1 <- plot_triangles(Iteration_1)
#' plot_1 
plot_triangles <- function(list_triangles) {
  df <- data.frame()
  
  #Création d'un df avec les coordonnés des triangles
  for (i in seq_along(list_triangles)) {
    df <- rbind(df, data.frame(
      x = c(list_triangles[[i]][1], list_triangles[[i]][3], list_triangles[[i]][5]),
      y = c(list_triangles[[i]][2], list_triangles[[i]][4], list_triangles[[i]][6]),
      triangle_id = i
    ))
  }
  
  #Représentation graphique
  plot <- ggplot(df, aes(x, y, fill = factor(triangle_id))) +
    geom_polygon(color = "black") +
    scale_fill_manual(values = rep("black", length(list_triangles)), guide = "none") +
    theme_void()
  
  return(plot)
}


##Fonctions : Calcul longueur + aire##


#' Calcul de a longueur entre deux points
#'
#' @param xA Coordonnée x du premier point
#' @param yA Coordonnée y du premier point
#' @param xB Coordonnée x du deuxième point
#' @param yB Coordonnée y du deuxième point
#'
#' @return La longueur entre deux points
#' @export
#'
#' @examples
#' longueur(0, 0, 3, 4)
longueur <- function(xA,yA,xB,yB){
  AB2 <- (xB-xA)^2 + (yB - yA)^2
  AB <- sqrt(AB2)
  return(AB)
}

#' Calcul de l'aire d'une liste de triangles
#'
#' @param list_triangles une liste de triangles
#'
#' @return L'aire totale des triangles de la liste
#' @import heron
#' @export
#'
#' @examples
#' list_triangles <- list(c(0, 0, 3, 0, 0, 4), c(1, 1, 4, 1, 1, 5))
#' calcul_aire(list_triangles)
calcul_aire = function(list_triangles){
  somme = 0
  for(i in seq_along(list_triangles)){
    AB <- longueur( list_triangles[[i]][1], list_triangles[[i]][2],
                          list_triangles[[i]][3], list_triangles[[i]][4])
    BC <- longueur( list_triangles[[i]][3], list_triangles[[i]][4],
                          list_triangles[[i]][5], list_triangles[[i]][6])
    AC <- longueur( list_triangles[[i]][1], list_triangles[[i]][2],
                          list_triangles[[i]][5], list_triangles[[i]][6])
    aire <- heron_bis(AB,BC,AC)
    somme <- somme + aire
  }
  return(somme)
}

