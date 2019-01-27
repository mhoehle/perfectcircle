# perfectcircle

A packaged version of the functionality described in the blog post [Judging Freehand Circle Drawing Competitions](http://staff.math.su.se/hoehle/blog/2018/07/31/circle.html).

To demo the package run the accompanying Shiny app by:

    devtools::install_github("hoehleatsu/perfectcircle")
    library(perfectcircle)
    perfectcircle::runExample()

The [Image file](https://github.com/hoehleatsu/perfectcircle/blob/master/inst/extdata/AlexOverwijk.jpg) and [CSV file](https://raw.githubusercontent.com/hoehleatsu/perfectcircle/master/inst/extdata/AlexOverwijk.csv) from the package's `extdata` folder can be downloaded to your local machine to test the app. Currently, the Shiny file selector does not allow the specification of URLs. The two files correspond to the setting used in the blog post.

Instead of installing the package one can also run the Shiny App directly from
[http://michaelhoehle.eu/shiny/perfectcircle/](http://michaelhoehle.eu/shiny/perfectcircle/).
