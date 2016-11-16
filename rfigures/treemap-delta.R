################################################################################
# Helper function using hackish SVG method to turn a single treemap SVG
# into a one, two, more "delta treemaps" showing changes
################################################################################

library(xml2)

calc_treemap_max <- function(data, vSize) {
  do.call(pmax, data[,vSize])
}

create_treemap_delta <- function(svg_out, svg_in, t, data, vSize, rm.aggregate.labels = T, label.only = NULL) {
  # extract our index from the treemap object
  lastIndex = which(colnames(t$tm)=="vSize") - 1
  index = colnames(t$tm)[1:lastIndex]
  
  # merge our data into the treemap data so we match output order
  df <- merge(t$tm, data, by=index)
  
  xml <- read_xml(svg_in)
  for (i in 1:nrow(df)) {
    rect <- xml_find_all(xml, "./*")[[1+i*2]]
    
    # max rect
    maxrect <- xml_add_sibling(rect, rect, .where = "before")
    style <- xml_attr(maxrect, "style")
    style_parts <- unlist(strsplit(style, ";", fixed=T))
    styles <- strsplit(style_parts, ":", fixed=T)
    fill_opacity <- which(sapply(styles, function(x) {x[1]=="fill-opacity"}))
    styles[[fill_opacity]][2] <- "0.15"
    style_parts <- sapply(styles, function(x) paste0(x, collapse=":"))
    style <- paste0(style_parts, collapse=";")
    xml_attr(maxrect, "style") <- style
    
    # regular rect
    a <- xml_attrs(rect)
    
    scale <- sqrt(df[i, vSize] / df[i, "vSize"])
    centre.x <- as.numeric(a['x']) + as.numeric(a['width'])/2.0
    centre.y <- as.numeric(a['y']) + as.numeric(a['height'])/2.0
    
    new.width <- as.numeric(a['width']) * scale
    new.height <- as.numeric(a['height']) * scale
    
    new.x <- centre.x - new.width/2.0
    new.y <- centre.y - new.height/2.0
    
    a[['x']] <- new.x
    a[['y']] <- new.y
    a[['width']] <- new.width
    a[['height']] <- new.height
    
    xml_attrs(rect) <- a
  }
  
  # Remove small labels
  label_nodes <- xml_find_all(xml, paste0(xml_path(rect),"/following-sibling::*"))
  part <- "rects"
  for (i in 1:length(label_nodes)) {
    n <- label_nodes[[i]]
    #print(part)
    #print(n)
    if (part == "rects") {
      if (xml_name(n) == "rect") {
        xml_remove(n)
      } else {
        part <- "labels"
      }
    }
    if (part == "labels") {
      if (xml_name(n) == "text") {
        if (!is.null(label.only)) {
          if (!(xml_text(n, trim=T) %in% label.only)) {
            xml_remove(n)
          }
        }
      } else {
        part <- "morerects"
      }
    }
    if (part == "morerects") {
      if (xml_name(n) == "rect") {
        xml_remove(n)
      } else {
        part <- "moretext"
      }
    }
    if (part == "moretext") {
      xml_remove(n)
    }
  }
  
  write_xml(xml, svg_out)
}