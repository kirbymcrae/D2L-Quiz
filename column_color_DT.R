column_cond_format <- function(x, upbnd = max(x), lowbnd = min(x), step_inc = .05, pal = "RdBu", alpha = .5, flip = FALSE) {

  stop_color_max <- upbnd
  stop_color_min <- lowbnd

  brks <- quantile(c(stop_color_min, stop_color_max), probs = seq(0 + step_inc, 1 - step_inc, step_inc), na.rm = TRUE)

  if (length(brks) + 1 <= RColorBrewer::brewer.pal.info$maxcolors[rownames(RColorBrewer::brewer.pal.info) == pal]) {
    myramp <- RColorBrewer::brewer.pal(pal, length(brks) - 1)
  } else {
    rcbramp <- RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info$maxcolors[rownames(RColorBrewer::brewer.pal.info) == pal], pal)
    myramp <- colorRampPalette(rcbramp)(length(brks) - 1)
  }
  clrs <- c(myramp[1], myramp, myramp[length(myramp)])

  if(flip == TRUE){
    clrs <- rev(clrs)
  }

    alphaclr <- col2rgb(clrs, alpha=T)

    alphaclr[4,] <- alpha*255
    alphaclr <- alphaclr/255.0
    alphaclr <- rgb(alphaclr[1,], alphaclr[2,], alphaclr[3,], alphaclr[4,])

  return(list("cuts" = brks, "values" = alphaclr))
}

# column_cond_format <- function(x, step_inc = .025, pal = "RdBu") {
#   stop_color_max <- max(x)
#   stop_color_min <- min(x)
#   brks <- quantile(c(stop_color_min, stop_color_max), probs = seq(0 + step_inc, 1 - step_inc, step_inc), na.rm = TRUE)
#   myPal <- shades::gradient(pal, length(brks) - 1, space = "Lab")
#
#   if(length(brks) + 1 <= RColorBrewer::brewer.pal.info$maxcolors[rownames(RColorBrewer::brewer.pal.info) == pal]){
#
#     myramp <- RColorBrewer::brewer.pal(pal, length(brks)-1)
#
#   }else{
#
#     mypal <- RColorBrewer::brewer.pal(pal, RColorBrewer::brewer.pal.info$maxcolors[rownames(RColorBrewer::brewer.pal.info) == pal])
#     myramp <- colorRampPalette(mypal)(length(brks)-1)
#
#   }
#   clrs <- c(myPal[1], myPal, myPal[length(myPal)])
#
#   return(list('cuts' = brks, 'values' = clrs))
# }
