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

##Server function
server <- function(input, output, session) {
  ##Create reactive values
  v <- reactiveValues(img_path = NULL, seedPoints= NULL, scaleFactor= NULL, go= FALSE)

  # getCircImg <- eventReactive(input$myFileCirc, {
  #   return( input$myFileCirc$datapath )
  # })
  # getSeedPoints <- eventReactive(input$myFileSeedPoints, {
  #   seedPoints <- read.csv(input$myFileSeedPoints$datapath, encoding="UTF8")
  #   names(seedPoints) <- NULL
  #   return(seedPoints)
  # })
  # getScaleFactor <- eventReactive(input$scaleFactor, {
  #   return(input$scaleFactor)
  # })
  #
  # getGoAction <- eventReactive(input$goButton, {
  #   return(TRUE)
  # })

  observeEvent(input$myFileCirc, {
    v$img_path <- input$myFileCirc$datapath
  })
  observeEvent(input$myFileSeedPoints, {
    seedPoints <- read.csv(input$myFileSeedPoints$datapath, encoding="UTF8")
    names(seedPoints) <- NULL
    v$seedPoints <- seedPoints
  })
  observeEvent(input$scaleFactor, {
   v$scaleFactor <- input$scaleFactor
  })

  # getGoAction <- eventReactive(input$goButton, {
  #   return(TRUE)
  # })
  observeEvent(input$goButton, {
    v$go <- input$goButton
  })

  output$result_producer <- renderUI({
    ##Trigger only if there is action.
    print(v$go)
    if (!v$go) return()

    # Create a Progress object - https://shiny.rstudio.com/articles/progress.html
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Measuring", detail="Scaling Image...", value = 0)

    ##Extract seed points and image to go from.
    seedPoints <- v$seedPoints #getSeedPoints()

    ##Loading image
    # #print(getCircImg())
    # img <- imager::load.image(file=getCircImg())
    # print(img)
    print(v$img_path)
    img <- imager::load.image(file=v$img_path)
    print(img)

    ##
    scaleFactor <- v$scaleFactor
    print(scaleFactor)

    ##Scale image and coordinates
   ## browser()
    #img <- imager::resize(img, -getScaleFactor(), -getScaleFactor())
    img <- imager::resize(img, -scaleFactor, -scaleFactor)
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
               column(3,fileInput("myFileCirc", "Choose a picture to measure circularity in", accept = c('image/png', 'image/jpeg'))),
               column(3,fileInput("myFileSeedPoints", "Choose a CSV file containing seed points", accept = c('text/csv'))),
               column(3, sliderInput("scaleFactor","Scale Factor",min=25, max=100, value=100, step = 25, post="%")),
               column(2, fluidRow(h3("")), fluidRow(h2("")), fluidRow(actionButton("goButton", "Go!")),offset=1)

             ),
             hr(),
             h3("Circularity Score:"),
             htmlOutput("result"),
             htmlOutput("result_producer"),
             hr(),
             imageOutput("image_result")
    ),#,# end tabPanel
    tabPanel("Details",
             imageOutput("image_details"),
             "Note: Both images are shown at a 25% scale of the size used in the calculations."
    ),
    tabPanel("Help",
             h2("The App"),
             "The R source code of ", em("The perfect circle"), " shiny app is available as part of the R package ", em("perfectcircle"), " available from ", a(href="https://github.com/hoehleatsu/perfectcircle", "github"), "under a GPLv3 license. Please posts comments and bugs as github ", a(href="https://github.com/hoehleatsu/perfectcircle/issues", "issues"), ".",                                                                                                                                                                                                                                                                        HTML("</center>"),
             "More information about the algorithm as well as the manual of the app can be found as part of the R package ", a(href="https://github.com/hoehleatsu/perfectcircle", em("perfectcircle")), ", e.g., on the Wiki page.",br(),
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
             "For the image the coordinates of at least two background and two foreground points have to be specified in a .csv file with columns 'x', 'y' and 'type' (where type is either: foreground or background). These coordinates can be found with any simple image program, e.g. MS Paint or GIMP, or directly in R using the locator function. An example of the required .csv file is:",
             p(),
             em("x,y,type"), br(),
             em("1304,1368,background"), br(),
             em("232,416,background"), br(),
             em("304,1304,foreground"), br(),
             em("2384,1304,foreground"),
             p(),
             "Note: UTF8 encoding is assumed for the CSV file. ",
             p(),
             h5("Result:"),
             "Once you uploaded the two files and selected the scale factor you have to hit the 'Go!' button to start the computation. This will compute the circularity score by comparing with the perfect circle. The reported score is the ", code("(1-ratio_areadifference)*100%"), " score described in ", a(href="http://staff.math.su.se/hoehle/blog/2018/07/31/circle.html", em("Judging Freehand Circle Drawing Competitions")), ". Details of the resulting fit are shown in the 'Details' panel. This shows for example how the extraction of the circle from the background worked and if, possibly, more foreground or background seed points are needed in the .csv file to get better results. It is possible to change just the .csv file, upload it again and hit 'Go!' to improve the detection sequentially.",
             p()
    )
  )
)

# Run the application
shinyApp(ui = ui, server = server)

