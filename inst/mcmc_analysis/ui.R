#-------------------------------------------
# ui.R

library(shiny)

shinyUI(
    fluidPage(
        title = "MCMC Shiny",
        #-------------------------------------------
        # CSS Style
        tags$head(
            tags$link(
                 rel = "stylesheet",
                 type = "text/css",
                 href = "style.css")
            ),

        #-------------------------------------------
        # Header
        htmlOutput("header"),

        sidebarLayout(
            #-------------------------------------------
            # Controls Panel
            sidebarPanel(
                width = 3,
                tabsetPanel(
                    id = "grandTab",
                    type = "pills",
                    tabPanel(
                        title = "Setup",
                        hr(),
                        uiOutput("UIsetup")
                    ),
                    tabPanel(
                        title = "Diagnosis",
                        hr(),
                        uiOutput("UIdiagnosis"),
                        uiOutput("UIdiagnosis_options")
                    ),
                    tabPanel(
                        title = "Analysis"
                    )
                ),
                verbatimTextOutput("test")
            ),

            #-------------------------------------------
            # Output Panel
            mainPanel(
                width = 9,
                uiOutput("UImain")
            )
        )
    )
)
