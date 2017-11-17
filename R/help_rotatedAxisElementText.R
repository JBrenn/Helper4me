#' @title Return Element Text Object
#' @description Return Element Text Object in pecific angles
#' @param angle angle in which to turn element [0;360]
#' @param position axis position, Default: 'x'
#' @return
#' @details
#' @examples
#' \dontrun{
#' if(interactive()){
#' #Load Required Libraries
#'library(ggplot2)
#'library(gridExtra)
#'  #Demonstrate Usage for a Variety of Rotations
#'df    = data.frame(x=0.5,y=0.5)
#'plots = lapply(seq(0,90,length.out=4),function(a){
#'  ggplot(df,aes(x,y)) +
#'    geom_point() +
#'    theme(axis.text.x = rotatedAxisElementText(a,'x'),
#'          axis.text.y = rotatedAxisElementText(a,'y')) +
#'    labs(title = sprintf("Rotated %s",a))
#'})
#'grid.arrange(grobs=plots)
#'  }
#' }
#' @rdname rotatedAxisElementText
#' @export rotatedAxisElementText
#Build Function to Return Element Text Object
rotatedAxisElementText = function(angle,position='x'){
  angle     = angle[1];
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust)
}
