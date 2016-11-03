
library(sp)
library(rgeos)

slice_world <- function(sp, longitude) {
  for (i in 1:nrow(sp)) {
    target <- sp[i,]
    line <- SpatialLines(list(Lines(list(Line(cbind(c(longitude,longitude),c(-90,90)))), ID="slice")), proj4string = target@proj4string)
    slice <- gIntersection(target, line)
    if (!is.null(slice)) {
      buf <- gBuffer(slice, width = 0.001)  # create a very thin polygon 
      new_target <- gDifference(target, buf)                # split using gDifference 
      new_target@polygons[1][[1]]@ID <- sp@polygons[i][[1]]@ID
      sp@polygons[i] <- new_target@polygons
    }
  }
  return(sp)
}