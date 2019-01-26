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
  getCircImg <- eventReactive(input$myFileCirc, {
    return( input$myFileCirc$datapath )
  })

  getSeedPoints <- eventReactive(input$myFileSeedPoints, {
    seedPoints <- read.csv(input$myFileSeedPoints$datapath, encoding="UTF8")
    names(seedPoints) <- NULL
    return(seedPoints)
  })


  output$result <- renderUI({
    ##Extract seed points and image to go from.
    seedPoints <- getSeedPoints()

    ##Loading image
    print(getCircImg())
    img <- imager::load.image(file=getCircImg())
    print(img)

    # Create a Progress object - https://shiny.rstudio.com/articles/progress.html
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "Measuring", value = 0)

    ##Measure circularity (show progress bar if within shiny)
    res <- circularity( img, seedPoints=getSeedPoints(), progress=progress)

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

    print(res)

    HTML(paste0("<h2><b>",sprintf("%.2f%%",res$score),
                '</b></h2><font color="gray">(area ratio: ',sprintf("%.4f",res$ratio_area),")</font>"))

  }) #renderUI
}

ui <-  fluidPage(
  # Application title
  titlePanel("The perfect circle!"),
  "By ",a(href="http://www.math.su.se/~hoehle", "M. Höhle"),
  "based on the algorithm described in ", a(href="http://staff.math.su.se/hoehle/blog/2018/07/31/circle.html", em("Judging Freehand Circle Drawing Competitions")), ", but without the rectifcation step.",
  p(),p(),
  # Show a plot of the generated distribution
  tabsetPanel(
    tabPanel("Main",
             h1(""),
             fluidRow(
               column(4,fileInput("myFileCirc", "Choose a picture to measure circularity in", accept = c('image/png', 'image/jpeg'))),
               column(4,fileInput("myFileSeedPoints", "Choose a CSV file containing seed points", accept = c('text/csv')))
             ),
             hr(),
             h3("Circularity Score:"),
             htmlOutput("result"),
             hr(),
             imageOutput("image_result")
    ),#,# end tabPanel
    tabPanel("Details",  imageOutput("image_details")),
    tabPanel("Help",
             h2("The App"),
             "The R source code of the perfect circle shiny app is available as part of the R package ", em("perfectcircle"), " available from ", a(href="https://github.com/hoehleatsu/perfectcircle", "github"), "under a GPLv3 license. Please posts comments, bugs and issues there.",br(),
             HTML("<center>"),
             img(src="adam-lowe-Shiny-Blue-Circle.png"), #https://openclipart.org/detail/23394/shiny-blue-circle,
             HTML("</center>"),
             h2("Freehand Circle Drawing Rules"),
             "The rules are described in a ", a(href="http://slamdunkmath.blogspot.com/2017/01/15-minutes.html","Judging Freehand Circle Drawing Competitions"), "by the first World Champion in Freehand Circle Drawing: ", a(href="https://twitter.com/AlexOverwijk","Alexander Overwijk"),".",
             HTML("<center>"),
             img(src="adam-lowe-Shiny-Blue-Circle.png"), #https://openclipart.org/detail/23394/shiny-blue-circle,
             HTML("</center>"),
             h2("World Championship in Freehand Circle Drawing"),
             "Are you better than the World Champion? If you think so then provide proof by making a github pull request containing your named .jpg and .csv files to the ", a(href="https://github.com/hoehleatsu/perfectcircle/tree/master/inst/extdata", "extdata"), "github folder of the package.",
             HTML("<center>"),
             img(src="adam-lowe-Shiny-Blue-Circle.png"), #https://openclipart.org/detail/23394/shiny-blue-circle,
             HTML("</center>"),
             h2("Using the Shiny App"),
             h5("Image:"),
             "Simply select a rectified .png or .jpg image of your circle using the file selector in the shiny app. Your image should be rectified - that means parallel lines should appear as parallel lines and 90 degree angles should really be visible as 90 degree angles in the image. Such a picture can for example be obtained by placing the camera on a pod a few meters away from the center of your drawing canvas and ensure a constant lighting without reflections.  Best results are obtained if there is a large contrast between circle and background (e.g. white chalk on a clean blackboard). The more pixels the uploaded contains the longer the computations take. It can thus be a good idea to use only medium-resolution images, either by downscaling the original images or by taking the photos with a medium resolution in the first place.",
             p(),
             h5("Seed Points:"),
             "For the image the coordinates of at least two background and two foreground points have to be specified in a .csv file with columns x, y and type (where type is either: foreground or background). These coordinates can be found with any simple image program, e.g. MS Paint or GIMP. For example:",
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
             "Once you uploaded the two files the program will start automatically and compute the circularity score by comparing with the perfect circle. The reported score is the ", code("(1-ratio_areadifference)*100%"), " score described in the above mentioned blog post. Details of the fit can be found in the 'Details' pane. This can for example be used to see if more foreground or background seed points are needed to get better results.",
             p()
    )
  )
)

# Run the application
shinyApp(ui = ui, server = server)

