#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# Author: Michael Höhle <http://www.math.su.se/~hoehle>
# Date:   24 Jan 2019
#
# Description: Shiny web app based on the algorithms described in the blog post
# http://staff.math.su.se/hoehle/blog/2018/07/31/circle.html .
# For feedback and comments use the github repository specified below.
#
# Url: https://github.com/hoehleatsu/perfectcircle
#
# License: GNU General Public License (GPL v3 - https://www.gnu.org/licenses/gpl.html)
##
library("shiny")
library("perfectcircle")
library("ggplot2")
library("dplyr")

##Server function
server <- function(input, output, session) {
  ##Create reactive values
  v <- reactiveValues(img_path = NULL, img=NULL, img_df = NULL, file_name = NULL, seedPoints= NULL, scaleFactor= NULL, go= FALSE)
  ##Global variable
  seedPoints_working <<- NULL

  make_original_panel <- function() {
    cat("Running 'make_original_panel'...\n")
    if (is.null(v$img)) return(NULL)

    v$img_df <- v$img %>% imager::resize(-v$scaleFactor, -v$scaleFactor) %>%
      grayscale() %>% as.data.frame() %>%
      mutate(x=x*100/v$scaleFactor, y=y*100/v$scaleFactor)

    print(dim(v$img_df))

    ##Render the 'original' plot.
    output$plot_original <- renderPlot({
      p <- ggplot(v$img_df,aes(x=x,y=y)) + geom_raster(aes(fill=value)) +
      {if (!is.null(v$seedPoints)) geom_point( data=v$seedPoints, aes(x=x, y=y, color=type))} +
        scale_x_continuous(expand=c(0,0)) +
        scale_y_continuous(expand=c(0,0),trans=scales::reverse_trans()) +
        scale_fill_gradient(low="black",high="white") +
        coord_fixed() +
        guides(fill=FALSE, color=FALSE)
      p
    })
    invisible()
  }

  observeEvent(input$myFileCirc, {
    v$img_path <- input$myFileCirc$datapath
    v$file_name <- input$myFileCirc$name
    print(v$file_name)

    ##Loading image
    v$img <- imager::load.image(file=v$img_path)
    print(v$img)

    ##Make original panel
    make_original_panel()

    ##Done with actions on load image
  })

  observeEvent(input$myFileSeedPoints, {
    seedPoints <- read.csv(input$myFileSeedPoints$datapath, encoding="UTF8", stringsAsFactors = FALSE)
    v$seedPoints <- seedPoints
  })
  observeEvent(input$scaleFactor, {
   v$scaleFactor <- input$scaleFactor
   ##Make original panel based on this scale factor.
   make_original_panel()
  })
  observeEvent(input$goButton, {
    v$go <- input$goButton
  })
  observeEvent(input$resetSeedPointsButton, {
    v$seedPoints <- NULL
  })
  # Downloadable csv of selected dataset ----
  output$downloadSeedPoints <- downloadHandler(
    filename = function() {
     gsub("\\.jpg$","\\.csv", v$file_name)
    },
    content = function(file) {
      write.csv(v$seedPoints, file, row.names = FALSE)
    }
  )

  output$result_producer <- renderUI({

    ##Click action
    output$click_info <- renderTable({
      # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
      # were a base graphics plot, we'd need those.
      print("Input pixeltype:")
      print(input$pixel_type)
      print(input$plot_click)

      print("Click Action:")
      print(input$plot_click)

      if (!is.null(input$plot_click)) {
        if (input$pixel_type %in% c("foreground", "background")) {
          print("Seedpoints:")
          print(v$seedPoints)
          pts <- nearPoints(v$img_df, input$plot_click, addDist = FALSE, maxpoints=1) %>% select(x,y)
          pts <- pts %>% mutate(type = input$pixel_type)
          print("Points")
          print(pts)
          #browser()
           if (is.null(v$seedPoints)) {
             v$seedPoints <- pts
           } else {
            v$seedPoints <- full_join(v$seedPoints, pts, by=c("x","y","type"))
          }
        } else {
          if (input$pixel_type %in% c("delete")) {
            pts <- nearPoints(v$seedPoints, input$plot_click, addDist = FALSE, maxpoints=1) %>% select(x,y)
            v$seedPoints <- anti_join(v$seedPoints, pts, by=c("x","y"))
          }
        }
      }
      return(v$seedPoints)
    })

    ##Trigger only if there is action.
    print(v$go)

    #browser()

    if (!v$go) return()
    if (is.null(v$seedPoints) || nrow(v$seedPoints) == 0) return(simpleError(message="Problem! No seedpoints."))

    # Create a Progress object - https://shiny.rstudio.com/articles/progress.html
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Measuring", detail="Scaling Image...", value = 0)

    ##Extract seed points and scale factor.
    seedPoints <- v$seedPoints
    scaleFactor <- v$scaleFactor
    print(scaleFactor)

    ##Scale image and coordinates
    img <- imager::resize(v$img, -scaleFactor, -scaleFactor)
    print(img)
    print(seedPoints)
    print(scaleFactor)
    seedPoints[,1:2] <- as.matrix(seedPoints[,1:2]) * scaleFactor/100
    print(seedPoints)

    ##Measure circularity (show progress bar if within shiny)
    res <- circularity( img, seedPoints=seedPoints, progress=progress)

    output$image_result <- renderImage({
      # Return a list containing the filename
      list(src = res$outfile,
           contentType = 'image/jpg',
           alt = "Image shows the extracted freehand circle contrasted to the perfect circle fitted to the points")
    }, deleteFile = TRUE)

    output$image_details <- renderImage({
      # Return a list containing the filename
      list(src = res$outfile_intermediate,
           contentType = 'image/jpg',
           alt = "Image shows the extracted circle and background")
    }, deleteFile = TRUE)

    output$result <- renderUI({
      HTML(paste0("<h2><b>",sprintf("%.2f%%",res$score),
                   '</b></h2><font color="gray">(area ratio: ',sprintf("%.4f",res$ratio_area),")</font>"))
    })

    print(res)

    v$go <- FALSE

    HTML("")
  }) #renderUI
}

ui <-  fluidPage(
  # Application title
  titlePanel("The perfect circle!"),
  "By ",a(href="http://www.math.su.se/~hoehle", "M. Höhle"),
  "based on the algorithm described in ", a(href="http://staff.math.su.se/hoehle/blog/2018/07/31/circle.html", em("Judging Freehand Circle Drawing Competitions")), ".",
  p(),p(),
  # Show a plot of the generated distribution
  tabsetPanel(
    tabPanel("Main",
             h1(""),
             fluidRow(
               column(4,fileInput("myFileCirc", "Select image to analyse:", accept = c('image/png', 'image/jpeg'))),
               #column(3,fileInput("myFileSeedPoints", "Choose a CSV file containing seed points", accept = c('text/csv'))),
               column(3, sliderInput("scaleFactor","Scale Factor",min=10, max=100, value=100, step = 10, post="%")),
               column(2, fluidRow(h3("")), fluidRow(h2("")), fluidRow(actionButton("goButton", "Go!")),offset=1)

             ),
             hr(),
             h3("Circularity Score:"),
             htmlOutput("result"),
             htmlOutput("result_producer"),
             hr(),
             imageOutput("image_result")
    ),#,# end tabPanel
    tabPanel("Seed points",
             plotOutput("plot_original", click = clickOpts("plot_click")),
             radioButtons("pixel_type", "Action on click:",
                          c("Add circle point" = "foreground",
                            "Add background point" = "background",
                            "Delete point" = "delete"), inline=TRUE),
             hr(),
             fluidRow(
               column(4, fileInput("myFileSeedPoints", "Upload:", accept = c('text/csv'))),
               column(2, h1(""), actionButton("resetSeedPointsButton", "Reset")),
               column(2, h1(""), downloadButton("downloadSeedPoints", "Download"))
             ),
             hr(),
             "The points:",
             #verbatimTextOutput("click_info")
             tableOutput("click_info")
    ),
    tabPanel("Fit Details",
             imageOutput("image_details"),
             "Note: Both images are shown at a 25% scale of the size used in the calculations."
    ),
    tabPanel("Help",
             h2("The App"),
             "The R source code of ", em("The perfect circle"), " shiny app is available as part of the R package ", em("perfectcircle"), " available from ", a(href="https://github.com/hoehleatsu/perfectcircle", "github"), "under a GPLv3 license. Please posts comments and bugs as github ", a(href="https://github.com/hoehleatsu/perfectcircle/issues", "issues"), ".",                                                                                                                                                                                                                                                                        HTML("</center>"),
             "More information about the algorithm as well as the manual of the app can be found as part of the R package ", a(href="https://github.com/hoehleatsu/perfectcircle", em("perfectcircle")), ", e.g., on the Wiki page. Furthermore, two video tutorials of the shiny app are described in the blog post ", a(href="http://staff.math.su.se/hoehle/blog/2019/02/15/shinycircle.html", "A Shiny app for your perfect circle"), ".", br(),
             HTML("<center>"),
             img(src="adam-lowe-Shiny-Blue-Circle.png"), #https://openclipart.org/detail/23394/shiny-blue-circle,
             HTML("</center>"),
             h2("World Championship in Freehand Circle Drawing"),
             "Can you draw freehand circles like a World Champion? If yes, prove it by making a github pull request containing the files [Alias].jpg and [Alias].csv files to the ", a(href="https://github.com/hoehleatsu/worldfreehandcirclechampionship/tree/master/round-1", "round-1"), " folder of the ", a(href="https://github.com/hoehleatsu/worldfreehandcirclechampionship", em("worldfreehandcirclechampionship")), "github project.",
             HTML("<center>"),
             img(src="adam-lowe-Shiny-Blue-Circle.png"), #https://openclipart.org/detail/23394/shiny-blue-circle,
             HTML("</center>"),
             h2("Using the Shiny App"),
             h5("Image:"),
             "Select a rectified .jpg image of your circle using the file selector of the shiny app. Your image should be ", strong("rectified")," - that means parallel lines should appear as parallel lines and 90 degree angles should display as 90 degree angles in the image. Such a picture can for example be obtained by placing the camera on a pod a few meters away from the center of your drawing canvas and on the line of an orthogonal vector from this center. Also ensure a constant lighting without reflections.  Best results are obtained if there is a large contrast between circle and background, e.g., white chalk on a clean blackboard or black edding on a whiteboard. Note: The more pixels the uploaded contains the longer the computations take. It can thus be a good idea to use the ", em("scale factor"), " slider to reduce the resolution in order to speed up the computations. Warning: Results for an image may vary slightly based on the selected scale factor. If one wants to compare the score of several different circles one should compare the scores at the same scaling.",
             p(),
             h5("Seed Points:"),
             "For the image the coordinates of at least two background and at least one foreground points have to be specified: For the background points at least one has to be placed inside the circle and at least one outside the circle. The seedpoints are specified in the 'Seed points' tab bei either clicking in the image (choosing the appropriate 'click action') or by uploading a .csv file with columns 'x', 'y' and 'type' (where type is either: foreground or background). The later allows the user to find the coordinates using her favorite image analysis program, e.g., Gimp or MS paint or directly in R using the 'locator' function. An example of such a .csv file is:",
             p(),
             em("x,y,type"), br(),
             em("1304,1368,background"), br(),
             em("232,416,background"), br(),
             em("304,1304,foreground"), br(),
             em("2384,1304,foreground"),
             p(),
             "It is possible to download the current selection of seed points as a .csv file (select: 'Download') in order to speed up the process a second time or to batch process an entire stack of images directly in R. Note: Displaying the image might take a little while, if the resolution is high. It can be beneficial to use a scale factor of 25% or lower in the main tab, because the image displayed is shown using the scale factor also and, hence, is faster.",
             p(),
             h5("Result:"),
             "Once you uploaded the two files and selected the scale factor you have to hit the 'Go!' button to start the computation. This will compute the circularity score by comparing with the perfect circle. The reported score is the ", code("(1-ratio_areadifference)*100%"), " score described in ", a(href="http://staff.math.su.se/hoehle/blog/2018/07/31/circle.html", em("Judging Freehand Circle Drawing Competitions")), ". Details of the resulting fit are shown in the 'Fit Details' panel. This shows for example how the extraction of the circle from the background worked and if, possibly, more foreground or background seed points are needed to get better results. It is possible to change just the seed points and hit 'Go!' in order to improve the detection sequentially.",
             p()
    )
  )
)

# Run the application
shinyApp(ui = ui, server = server)

