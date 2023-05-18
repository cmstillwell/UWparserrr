#' Use/Load UW Color Palette
#'
#' @param palette A text string identifying the palette to loaded, one of the following: multicolor, compare1/2/2uw, pairwise
#'
#' @return A string vector of hexidecimal colors
#' @export
#'
use_UW_colors <- function(palette = "multicolor") {

  color.table <- list(
    multicolor = c("#c5050c","#0479a8","#604878","#6B9F25","#CC9900","#9B0000","#9F87B7"),
    compare1   = c("#c5050c","#494949","#494949","#494949","#494949","#494949","#494949"),
    compare2   = c("#c5050c","#0479a8","#494949","#494949","#494949","#494949","#494949"),
    compare2uw = c("#c5050c","#CC9900","#494949","#494949","#494949","#494949","#494949"),
    pairwise   = c("#c5050c","#9b0000","#0479a8","#025A7E","#9F87B7","#604878","#A9DB66","#6B9F25")
  )

  return(color.table[[as.name(palette)]])

}
