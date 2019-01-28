#' A package to compute circularity of free hand circles
#'
#' The initial approach towards scoring free hand circle orignates from the blog post \href{http://staff.math.su.se/hoehle/blog/2018/07/31/circle.html}{Judging Freehand Circle Drawing Competitions}. The code of this post now developed into the present package. It contains an API for the scoring algorithms and a shiny app, which can be used without any technical knowledge.
#' Please visit the \href{https://github.com/hoehleatsu/perfectcircle}{github} pages of the package.
#  Note: The code of this package uses the \code{imager} package by \href{https://barthesi.gricad-pages.univ-grenoble-alpes.fr/personal-website/publications/}{Simon Barthelmé}.
#' @keywords internal
#'
#' @examples
#' library(purrr)
#' library(dplyr)
#'
#' scaleFactor <- 50
#' img_list <- list.files( path=system.file("extdata", package="perfectcircle"), pattern="*.jpg")
#'
#' ##Compute the circularity scores for the entire batch of jpg files located in a certain directory.
#' leaderboard <- map_df(img_list, ~ {
#'    cat("Processing: ", .x, "\n")
#'    #Load image and coordinates
#'    file_path <- system.file("extdata", .x, package="perfectcircle")
#'    img <- imager::load.image(file_path)
#'    seedPoints <- read.csv(file=gsub("\\.jpg$","\\.csv", file_path))
#'    names(seedPoints) <- NULL
#'
#'    ##Scale
#'    img <- imager::resize(img, -scaleFactor, -scaleFactor)
#'    seedPoints[,1:2] <- as.matrix(seedPoints[,1:2]) * scaleFactor/100
#'
#'    ##Measure
#'    res <- perfectcircle::circularity(warp=img, seedPoints=seedPoints, progress=NULL)
#'    res$fileName <- gsub("\\.csv","", .x)
#'
#'    res
#' })
#'
#' #Leaderboard
#' leaderboard %>% arrange(desc(score)) %>% select(score, fileName, ratio_area)
"_PACKAGE"
