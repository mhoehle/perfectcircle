#' Estimate best radius given center of the circle in an image.
#'
#' @description Given a mask image containing the borderline of the circle and the center coordinates
#' one can compute for this configuration using the formula by Coope (1993).
#'
#' @details The function implements Formula 3 in Coope (1993) while using a mask image to hold the points.
#' Given the center \eqn{c} we can determine the radius as \eqn{r(c)=\frac{1}{m} \sum_{j=1}^m ||c-a_j||_2}{r(c)=1/m \sum_{j=1}^m (c-a_j)^2}.
#' @references Coope, I.D. 1993. “Circle Fitting by Linear and Nonlinear Least Squares.” Journal of Optimization Theory and Applications 76 (2): 381–88. doi:10.1007/BF00939613.
#'
#' @param center A vector of length two containing the (x,y) coordinates of the center
#' @param freehandCircleThinBorder An imager image mask with the extracted borderline of the freehand circle having a pixel value > 0
#' @param dist A distance matrix for each pixel with \code{freehandCircleThinBorder} > 0
#'
#' @return Returns the mean of the distance matrix, which is the radius of the circle fitting the points optimally.
#'
#' @export
radius_given_center <- function(center, freehandCircleThinBorder, dist=NULL) {
  if (is.null(dist)) {
    a <- as.matrix(imager::where(freehandCircleThinBorder > 0))
    dist <- sqrt((a[,1] - center[1])^2 + (a[,2] - center[2])^2)
  }
  return(mean(dist))
}

#' Target function of the total least squares criterion of Coope (1993)
#'
#' @details
#' Denoting by \eqn{c} the center of the circle and by \eqn{r>0} the radius we
#' want to find the circle for which the Euclidean distance to $m$ data points $a_j$, $j=1,\ldots,m$ using the following criterion:
#' \deqn{\min_{c\in \mathbb{R^2}, r>0} \sum_{j=1}^m F_j(c,r)^2}, where \deqn{F_j(c,r) = \left|r - ||c-a_j||_2\right|}.
#' This is equation (1) and (2) of Coope (1993).
#'
#' @references Coope, I.D. 1993. “Circle Fitting by Linear and Nonlinear Least Squares.” Journal of Optimization Theory and Applications 76 (2): 381–88. doi:10.1007/BF00939613.

#' @param theta A vector of length 2 containing log of the center coordinates.
#' @return The total least squares \deqn{}{}
#'
#' @export

target_tls <- function(theta, freehandCircleThinBorder) {
  ##Extract parameters
  center <- exp(theta[1:2])

  ##Total least squares criterion from Coope (1993)
  a <- as.matrix(imager::where(freehandCircleThinBorder > 0))
  dist <- sqrt((a[,1] - center[1])^2 + (a[,2] - center[2])^2)
  ##Compute radius given center
  radius <- radius_given_center(center, freehandCircleThinBorder, dist)

  F <- abs( radius - dist)
  sum(F^2)
}


#' Helper function to rectify an image - currently not used
#' since points have to be transmitted.
#' @param img An image
#' @param p Control points

# rectify <- function(img, p) {
#   ## Control points in the image (found with Gimp or locator())
#  # p <- rbind(c(0,318),c(2016,100),c(1988,1242),c(0,980))
#
#   ## True coordinates of the control point, e.g. because we know its the corners of a 1m x 1m grid
#   dx_blackboard <- 1200
#   pp <- rbind(c(-dx_blackboard ,100),c(2016,100),c(1988,1242),c(-dx_blackboard,1242)) + matrix(c(dx_blackboard, 0),4,2,byrow=TRUE)
#
#   A <- list()
#   b <- list()
#
#   ##Build RHS and LHS matrix components for each point pair
#   for (i in 1:4) {
#     A[[paste0(i)]] <- matrix(c(
#       p[i,1],p[i,2],1, 0,      0,      0, -pp[i,1]*p[i,1], -pp[i,1]*p[i,2],
#       0,     0,     0, p[i,1], p[i,2], 1, -pp[i,2]*p[i,1], -pp[i,2]*p[i,2]),
#       2,8, byrow=TRUE)
#     b[[paste0(i)]] <- matrix(c(pp[i,1], pp[i,2]),2,1)
#   }
#
#   ##Glue matrices together
#   A_matrix <- do.call(rbind, A)
#   b_matrix <- do.call(rbind, b)
#
#   ##Solve equation system
#   h_vec <- solve(A_matrix, b_matrix)
#
#   ##Form H matrix from the solution
#   H <- matrix(c(h_vec,1), 3,3, byrow=TRUE)
#   H
#
#
#   ##Transform image coordinates (x',y') to (x,y), i.e. note we specify
#   ##the back transformation p = H * p', so H here is the inverse.
#   map.persp.inv <- function(x,y, H) {
#     out_image <- H %*% rbind(x,y,1)
#     list(x=out_image[1,]/out_image[3,], y=out_image[2,]/out_image[3,])
#   }
#   ##Pad dx_blackboard pixels to the right to make space for the blackboard coming closer
#   img_padded <- pad(img, nPix=dx_blackboard, axes="x", pos=1)
#   ##Warp image
#   warp <- imwarp(img_padded, map=function(x,y) map.persp.inv(x,y,solve(H)),coordinates="absolute", direction="backward")
#
#   ##Crop! Remove this part.
#   warp <- imsub(warp, x %inr% c(dx_blackboard, nrow(warp)))
#
#   ##Show before after
#   layout(c(1,2))
#   plot(img, main="Original")
#   lines(c(p[,1],p[1,1]), c(p[,2],p[1,2]), col="magenta",lwd=2)
#   points(p[,1], p[,2], cex=3, pch=20, col="steelblue")
#   plot(warp, main="Rectified")
#   lines(c(pp[,1],pp[1,1]), c(pp[,2],pp[1,2]), col="magenta",lwd=2)
#   points(pp[,1], pp[,2], cex=3, pch=20, col="steelblue")
#
#   ##Store rectified result
#   imager::save.image(warp, file="/Users/hoehle/Sandbox/ShinyCircles/rectified.jpg")
#
#   return(warp)
# }

#' Helper - don't export
my_incProgress <- function (progress, amount = 0.1, message = NULL, detail = NULL, session = getDefaultReactiveDomain()) {
  if (!is.null(progress)) {
    progress$inc(amount=amount, message=message, detail=detail)
  }
  invisible()
}

#' Measure circularity of a circle in an image
#'
#' @description Function executes the steps described in \href{http://staff.math.su.se/hoehle/blog/2018/07/31/circle.html}{Judging Freehand Circle Drawing Competitions}.
#' Note: Opposite to the description in the blog post we assume that the image is already rectified.
#'
#' @references Höhle, M. (2018), \href{http://staff.math.su.se/hoehle/blog/2018/07/31/circle.html}{Judging Freehand Circle Drawing Competitions}.
#'
#' @param warp The rectified image containing the circle
#' @param seedPoints A \code{data.frame} containing the (x, y) coordinates of \code{type=="foreground"} and \code{type=="background"} pixels.
#' @param progress A shiny progress bar.
#'
#' @return  A list containing several elements
#' \describe{
#'    \item{outfile}{File name (with path) of the image containing both the extracted freehand circle and the best fitting circle}
#'    \item{outfile_intermediate}{File name (with path) containing the intermediate results}
#'    \item{area}{Area of the disc made up by the freehand circle}
#'    \item{ratio_area}{Ratio between the above \code{area} and the corresponding area of the perfect circle. 1 is optimial}
#'    \item{area_difference}{Summing up all points inside and outside the perfect circle weighted by distance (see blog post for details)}
#'    \item{score}{Corresponds to \code{(1-ratio_areadifference)*100}. The optimal value is 100\%.}
#' }
#' @examples
#' img <- imager::load.image(  system.file("extdata/", "localhost.jpg", package = "perfectcircle") )
#' seedPoints <- read.csv( system.file("extdata", "localhost.csv", package="perfectcircle"))
#' res <- perfectcircle::circularity(img, seedPoints, progress=NULL)
#' res
#'
#' plot(imager::load.file(res$outfile))
#' @export

circularity <- function(warp, seedPoints, progress, verbose=FALSE) {
  ##Number of steps for the progress bar
  nSteps <- 8
  if (verbose) print(pryr::mem_used())

  my_incProgress(progress, 1/nSteps, detail='Detecting edges...')

  ##Edge detection function. Sigma is the size of the blur window.
  detect.edges <- function(im, sigma=1) {
    isoblur(im,sigma) %>% imgradient("xy") %>% enorm() %>% imsplit("c") %>% add
  }
  if (verbose) print(pryr::mem_used())
  #Edge detection filter sequence.
  edges <- detect.edges(warp,1) %>% sqrt

  ##Clean and progress
  rm(detect.edges)
  if (verbose) print(pryr::mem_used())

  if (verbose) my_incProgress(progress, 1/nSteps, detail = "Priority map...")

  #Priority map with priority inverse proportional to gradient magnitude
  pmap <- 1/(1+edges)

  ##Clean and progress
  rm(edges)
  if (verbose) print(pryr::mem_used())

  my_incProgress(progress, 1/nSteps, detail = "Watershedding...")

  ##Seedpoints
  background <- as.matrix(seedPoints[ seedPoints[,3] == "background", 1:2])
  foreground <- as.matrix(seedPoints[ seedPoints[,3] == "foreground", 1:2])

  ##Create seed image
  seeds <- imfill(dim=dim(pmap))
  seeds[cbind(background,1,1)] <- 1
  seeds[cbind(foreground,1,1)] <- 2

  ##Run watershed algorithm
  wt <- watershed(seeds, pmap)

  ##Clean and progress
  rm(seeds, pmap)
  if (verbose) print(pryr::mem_used())


  ##We copy along the three colour channels
  my_incProgress(progress, 1/nSteps, detail = "Converting to colour channels...")

  mask <- add.colour(wt)
  rm(wt)

  my_incProgress(progress, 1/nSteps, detail = 'Image for "Details"...')

  ##Extract parts - since this crashes for larger images we work
  ##with a 25% scaled images
  background_img <- resize(warp,-25,-25) * resize(mask==1, -25, -25)
  foreground_img <- resize(warp,-25,-25) * resize(mask==2, -25, -25)


  ##Store intermediate results in one png as a temp file to save
  ##the output, this file will be removed later by renderImage
  outfile_intermediate <- tempfile(fileext = '.png')
  ##outfile_intermediate <- "construction-icon.png"#

  ##Store details
  png(outfile_intermediate, width = 800, height = 400)
  layout(matrix(c(1,2), nrow=1, ncol=2, byrow=TRUE))
  plot(background_img, main="Background")
  points(background/4, col="steelblue", pch=20)
  plot(foreground_img, main="Foreground")
  points(foreground/4, col="magenta", pch=20)
  dev.off()


  rm(foreground_img, background_img)

  my_incProgress(progress, 1/nSteps, detail = "Extract circle...")
  ##Just the circle
  freehandCircle <- (warp * (mask==2) > 0) %>% grayscale
  ##Total area covered by the circle
  freehandDisc <- label(freehandCircle, high_connectivity=TRUE) > 0

  #Thin border
  dilatedDisc <- freehandDisc %>% dilate_rect(sx=3,sy=3)
  freehandCircleThinBorder <- (freehandDisc - dilatedDisc) != 0

  # par(mar=c(2,2,2,2))
  # plot(1-freehandCircleThinBorder, main="Extracted freehand circle")
  # ##done with the intermediate results - store to file.

  ##Area of the freehand drawn disc
  areaFreehandDisc <- sum(freehandDisc)

  ##Clean and progress
  rm(dilatedDisc, freehandDisc, freehandCircle, mask, warp)
  if (verbose) print(pryr::mem_used())

  ##Fit circle to points
  my_incProgress(progress, 1/nSteps, detail = "Fit circle...")
  res_tls <- optim(par=log(c(x=background[1,1], y=background[1,2])), fn=target_tls, freehandCircleThinBorder=freehandCircleThinBorder)
  center <- exp(res_tls$par)
  fit_tls <- c(center,radius=radius_given_center(center, freehandCircleThinBorder))
  fit_tls

  # A temp file to save the output.
  # This file will be removed later by renderImage
  outfile <- tempfile(fileext = '.png')
  ##Show fit in a png
  png(outfile, width = 800, height = 800)
  nPoints <- 1000
  t <- seq(0,2*pi, length=nPoints)
  circle <- cbind(fit_tls[3]*cos(t) + fit_tls[1], fit_tls[3]*sin(t)+fit_tls[2])
  plot(1-freehandCircleThinBorder, main="Freehand vs. perfect Circle")
  lines(circle[,1], circle[,2], col=rgb(0.8,0.4,0.9,0.3),lwd=3)
  points(fit_tls[1], fit_tls[2], pch=20, cex=1, col=rgb(0.8,0.4,0.9,0.4))
  dev.off()

  ######################################
  ## Calculation of the difference
  ######################################
  my_incProgress(progress, 1/nSteps, detail = "Calculate difference...")

  ##Area of the disc corresponding to the idealized circle fitted
  ##to the freehand circle
  areaIdealDisc <- pi * fit_tls["radius"]^2

  ##Ratio between the two areas
  ratio_area <- as.numeric(areaFreehandDisc  / areaIdealDisc)
  ratio_area

  ##Create a pixel based circle in an image of the same size as the
  ##freehandCircle img. For visibility we use a border of 'border' pixels
  ##s.t. circle goes [radius - border/2, radius + border/2].
  Circle <- function(center, radius, border) {
    as.cimg(function(x,y) {
      lhs <- (x-center[1])^2 + (y-center[2])^2
      return( (lhs >= (radius-border/2)^2) & (lhs <= (radius+border/2)^2))
    }, dim=dim(freehandCircleThinBorder))
  }

  ##Build pixel circle based on the fitted parameters
  C_tls <- Circle(fit_tls[1:2], fit_tls[3], border=1)
  ##Calculate Euclidean distance to circle for each pixel in the image
  dist <- distance_transform(C_tls, value=1, metric=2)
  ##Distance between outer border of freehand circle and perfect circle
  area_difference <- sum(dist[freehandCircleThinBorder>0])

  ##Compute area difference and scaled it by the area of the fitted disc
  ratio_areadifference <- as.numeric(area_difference / areaIdealDisc)

  rm(freehandCircleThinBorder, C_tls, dist)
  my_incProgress(progress, 1/nSteps, detail = "Done!")
  if (verbose) print(pryr::mem_used())

  ##Done
  res <- list(
    outfile=outfile,
    outfile_intermediate=outfile_intermediate,
    area=areaFreehandDisc,
    ratio_area=ratio_area,
    area_difference=area_difference,
    ratio_areadifference=ratio_areadifference,
    score=(1-ratio_areadifference)*100
  )

  ##Done
  return(res)
}

