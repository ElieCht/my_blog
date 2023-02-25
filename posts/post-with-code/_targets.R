# Created by use_targets().

library(targets)
library(tarchetypes)
source("posts/post-with-code/R/functions.R")

tar_option_set(
  packages = c("heron", "ggplot2", "devtools", "quarto", "ggplot2"))

#Q8
#Initialisation du triangle : A(0,0), B(0,1), C(0.5, sqrt(3)/2)
triangle <- c(0,1,0,0,0.5,sqrt(3)/2)

list(
  tar_target("div1", divide_triangle(triangle)),
  
  tar_target("plot1", plot_triangles(div1)),
  tar_target("aire1", calcul_aire(div1)),
  tar_target("first", divide_list_triangle(div1)),
  
  tar_target("plot2", plot_triangles(first)),
  tar_target("aire2", calcul_aire(first)),
  tar_target("second", divide_list_triangle(first)),
  
  tar_target("plot3", plot_triangles(second)),
  tar_target("aire3", calcul_aire(second)),
  tar_target("third", divide_list_triangle(second)),
  tar_target("plot4", plot_triangles(third)),
  
  tar_target("aire4", calcul_aire(third)),
  tar_target("Post_blog",tar_quarto(post.qmd))
)

