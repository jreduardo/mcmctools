#-------------------------------------------
# server.R

# Packages
library(shiny)
library(rjags)
library(lattice)
library(magrittr)
library(mcmctools)

# Datas
ps_lattice <- list(strip.background = list(col = "gray80"))
data("line", package = "coda")
object <- line

# Init the server
shinyServer(
    function(input, output, session) {
        #-------------------------------------------
        # Header
        output$header <- renderPrint({
            vers <- as.character(packageVersion("mcmctools"))
            tagList(
                h1(tag("code", "mcmctools"), "MCMC analysis app",
                   class = "title"),
                h4(paste("Installed version", vers),
                   class = "title"),
                hr()
            )
        })

        #-------------------------------------------
        # Controls fot setup tab
        output$UIsetup <- renderUI({
            vars <- unique(gsub("\\[.+\\]",
                                replacement = "",
                                varnames(object)))
            choices <- c("Choose variable" = "", "all", vars)
            selectInput(inputId = "VARS",
                        label = "Variables to analyse",
                        choices = choices,
                        multiple = TRUE)
        })

        #-------------------------------------------
        # General controls for diagnosis tab
        output$UIdiagnosis <- renderUI({
            choices <- c(
                "Trace of the chains"               = "trace",
                "Auto-correlations"                 = "autocorr",
                "Cross-correlations"                = "crosscorr",
                "Geweke diagnostic"                 = "geweke",
                "Gelman & Rubin diagnostic"         = "gelman",
                "Raftery and Lewis diagnostic"      = "raftery",
                "Heidelberger and Welch diagnostic" = "heidel")
            tagList(
                column(
                    width = 6,
                    numericInput(inputId = "BURNIN",
                                 label = "Burn-in",
                                 value = 0L,
                                 min = 0L,
                                 max = niter(object),
                                 step = 1L)
                ),
                column(
                    width = 6,
                    numericInput(inputId = "THIN",
                                 label = "Thinning",
                                 value = 1L,
                                 min = 1L,
                                 max = ceiling(niter(object)/10),
                                 step = 1L)
                ),
                column(
                    width = 12,
                    selectInput(inputId = "DIAG",
                                label = "Type of diagnosis",
                                choices = choices)
                )
            )
        })

        #-------------------------------------------
        # Options controls for selected diagnosis
        output$UIdiagnosis_options <- renderUI({
            if (is.null(input$DIAG)) {
                selected_diag <- "trace"
            } else {
                selected_diag <- input$DIAG
            }
            if (selected_diag == "trace") {
                tagList(
                    # column(
                    #     width = 12,
                    #     tag("label",
                    #         list("Window to show",
                    #              class = "control-label"))
                    # ),
                    column(
                        width = 6,
                        numericInput(inputId = "TRACE_NCOL",
                                     label = "Fig. columns",
                                     value = 1L,
                                     min = 1L,
                                     max = nvar(samples()))
                    ),
                    column(
                        width = 6,
                        numericInput(inputId = "TRACE_SIZE",
                                     label = "Size of window",
                                     value = 50L,
                                     min = 10L,
                                     max = niter(samples()))
                    ),
                    column(
                        width = 12,
                        uiOutput("UItrace_pos")
                    )
                )
            }
        })
        output$UItrace_pos <- renderUI({
            sliderInput(inputId = "TRACE_POS",
                        label = "Window to show",
                        value = input$TRACE_SIZE,
                        min = input$TRACE_SIZE,
                        max = niter(samples()),
                        step = input$TRACE_SIZE,
                        ticks = FALSE,
                        animate = list(interval = 500))
        })

        #-------------------------------------------
        # MCMC object
        samples <- reactive({
            if ("all" %in% input$VARS || is.null(input$VARS)) {
                vars <- 1:nvar(object)
            } else {
                vars <- input$VARS
            }
            if (is.null(input$BURNIN)) {
                burnin <- 0L
            } else {
                burnin <- input$BURNIN
            }
            if (is.null(input$THIN)) {
                thin <- 1L
            } else {
                thin <- input$THIN
            }
            object %>%
                mct_select(vars) %>%
                mct_burnin(burnin) %>%
                mct_thin(thin)
        })

        #-------------------------------------------
        # Main Panel
        output$UImain <- renderUI({
            if (input$grandTab == "Setup") {
                HTML("TODO MARKDOWN INFORMATION")
            }
            if (input$grandTab == "Diagnosis") {
                uiOutput("OUTdiagnosis")
            }
        })

        #-------------------------------------------
        # OUTPUT for Diagnosis
        output$OUTdiagnosis <- renderUI({
            plotOutput("traceplot")
        })

        output$traceplot <- renderPlot({
            if(is.null(input$TRACE_NCOL)) {
                layout <- c(1L, NA)
            } else {
                layout <- c(input$TRACE_NCOL, NA)
            }
            if(is.null(input$TRACE_POS)) {
                aux <- niter(samples())
            } else {
                aux <- input$TRACE_POS
            }
            index <- seq(aux - input$TRACE_SIZE + 1, aux, by = 1L)
            if (is.mcmc(samples())) {
                data <- as.mcmc(samples()[index, ])
            } else {
                data <- samples() %>%
                    lapply(function(x) as.mcmc(x[index, ])) %>%
                    as.mcmc.list()
            }
            xyplot(data,
                   as.table = TRUE,
                   layout = layout,
                   scales = list(
                       x = list(
                           at = pretty(1:input$TRACE_SIZE),
                           labels = pretty(index)
                       )
                   ),
                   par.settings = ps_lattice)
        }, height = 600)

        #-------------------------------------------
        # Only used for development
        output$test <- renderPrint({
            # str(input$TRACE_POS)
            # seq(input$TRACE_POS - input$TRACE_SIZE + 1, input$TRACE_POS, by = 1L)
            # str("all" %in% input$VARS)
            # mct_select(object, "")
            # input$VARS
        })
    }
)
