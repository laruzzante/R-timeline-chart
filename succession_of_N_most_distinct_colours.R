library(RColorBrewer)

get_colours <- function(n){
  qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
  colours = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
  pie(rep(1,n), col=colours)
  return(colours)
}
