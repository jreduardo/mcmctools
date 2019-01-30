#-------------------------------------------
# server.R

# Packages
library(shiny)
library(rjags)
library(lattice)
library(magrittr)
library(mcmctools)
library(latticeExtra)

# Trellis graphics style
ps_lattice <- list(strip.background = list(col = "gray80"))

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
        # Options controls for selected diagnosis
        output$UIdiagnosis_options <- renderUI({
            if (is.null(input$DIAG)) {
                selected_diag <- "trace"
            } else {
                selected_diag <- input$DIAG
            }
            if (selected_diag == "trace") {
                tagList(
                    column(
                        width = 6,
                        tag("label", "Show chains history"),
                        checkboxInput(inputId = "TRACE_all",
                                      label = "Show history",
                                      value = TRUE)
                    ),
                    column(
                        width = 6,
                        numericInput(inputId = "TRACE_SIZE",
                                     label = "Size of window",
                                     value = 10L,
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
        # Main Panel
        output$UImain <- renderUI({
            if (input$grandTab == "Setup") {
                h1("MCMC Tools - Shiny App")
            }
            else if (input$grandTab == "Diagnosis") {
                uiOutput("OUTdiagnosis")
            }
            # if (input$grandTab == "Setup") {
            #     # HTML("TODO MARKDOWN INFORMATION")
            #     plotOutput("traceplot")
            # }
            # if (input$grandTab == "Diagnosis") {
            #     uiOutput("OUTdiagnosis")
            # }
        })

        #-------------------------------------------
        # OUTPUT for Diagnosis
        output$OUTdiagnosis <- renderUI({
            if (input$DIAG == "trace") {
                uiOutput("UItrace")
            }
            else if (input$DIAG == "autocorr") {
                uiOutput("UIautocorr")
            }
            else if (input$DIAG == "crosscorr") {
                uiOutput("UIcrosscorr")
            }
            else if (input$DIAG == "geweke") {
                uiOutput("UIgeweke")
            }
            else if (input$DIAG == "gelman") {
                uiOutput("UIgelman")
            }
            else if (input$DIAG == "raftery") {
                uiOutput("UIraftery")
            }
            else if (input$DIAG == "heidel") {
                uiOutput("UIraftery")
            }
            else {
                h1("Choose a type of diagnosis")
            }
        })

        # UI for heidel
        output$UIheidel <- renderUI({
            tabsetPanel(
                tabPanel(
                    title = "Output",
                    verbatimTextOutput("heidelout")
                )
            )
        })

        # UI for rafery
        output$UIraftery <- renderUI({
            tabsetPanel(
                tabPanel(
                    title = "Output",
                    verbatimTextOutput("rafteryout")
                )
            )
        })

        # UI for gelman
        output$UIgelman <- renderUI({
            tabsetPanel(
                tabPanel(
                    title = "Plot",
                    plotOutput("gelmanplot")
                ),
                tabPanel(
                    title = "Output",
                    verbatimTextOutput("gelmanout")
                )
            )
        })

        # UI for geweke
        output$UIgeweke <- renderUI({
            tabsetPanel(
                tabPanel(
                    title = "Plot",
                    plotOutput("gewekeplot")
                ),
                tabPanel(
                    title = "Output",
                    verbatimTextOutput("gewekeout")
                )
            )
        })

        # UI for crosscor
        output$UIcrosscorr <- renderUI({
            tabsetPanel(
                tabPanel(
                    title = "Plot",
                    plotOutput("crosscorrplot")
                ),
                tabPanel(
                    title = "Output",
                    verbatimTextOutput("crosscorrout")
                )
            )
        })

        # UI for autocorr
        output$UIautocorr <- renderUI({
            tabsetPanel(
                tabPanel(
                    title = "Plot",
                    plotOutput("autocorrplot")
                ),
                tabPanel(
                    title = "Output",
                    verbatimTextOutput("autocorrout")
                )
            )
        })

        # UI for trace
        output$UItrace <- renderUI({
            tabsetPanel(
                tabPanel(
                    title = "Plot",
                    plotOutput("traceplot")
                )
            )
        })

        # Output for traceplot
        output$traceplot <- renderPlot({
            if(is.null(input$TRACE_POS)) {
                aux <- niter(samples())
            } else {
                aux <- input$TRACE_POS
            }
            tsize <- input$TRACE_SIZE
            index <- seq(aux - tsize + 1, aux, by = 1L)
            if (input$TRACE_all) {
                index <- seq(1L, niter(samples()), by = 1L)
                tsize <- niter(samples())
            }
            if (is.mcmc(samples())) {
                data <- as.mcmc(samples()[index, ])
            } else {
                data <- samples() %>%
                    lapply(function(x) as.mcmc(x[index, ])) %>%
                    as.mcmc.list()
            }
            layout <- if(nchain(data) == 1L) {
                          layout <- c(1L, NA)
                      } else layout <- c(2L, NA)
            # Plot
            xyplot(data,
                   as.table = TRUE,
                   layout = layout,
                   scales = list(
                       x = list(
                           at = pretty(1:tsize),
                           labels = pretty(index)
                       )
                   ),
                   par.settings = ps_lattice)
        }, height = 800)

        # Output for autocorrelations
        output$autocorrplot <- renderPlot({
            data <- samples()
            layout <- if(nchain(data) == 1L) {
                          layout <- c(1L, NA)
                      } else layout <- c(2L, NA)
            # Plot
            acfplot(data,
                    aspect = "fill",
                    ylim = c(-1, 1),
                    layout = layout,
                    type = "h",
                    par.settings = ps_lattice)
        }, height = 800)

        output$autocorrout <- renderPrint({
            autocorr(samples())
        })

        # Output for crosscorretations
        output$crosscorrplot <- renderPlot({
            data <- samples()
            par(mfrow = c(1, nchain(data)),
                mar = c(0, 2, 0, 1) + 0.1)
            for (i in seq_len(nchain(data))) {
                crosscorr.plot(data[[i]],
                               main = paste0("Chain", i))
            }
        }, height = 800)

        output$crosscorrout <- renderPrint({
            lapply(samples(), crosscorr)
        })

        # Output for geweke
        output$gewekeplot <- renderPlot({
            data <- samples()
            nv <- nvar(data)
            nc <- nchain(data)
            mat <- matrix(1:(nv * nc), nrow = nv, ncol = nc, byrow = )
            par(mar=c(2.6, 2.8, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
            layout(mat)
            geweke.plot(data, auto.layout = FALSE)
        }, height = 800)

        output$gewekeout <- renderPrint({
            geweke.diag(samples())
        })

        # Output for gelman
        output$gelmanplot <- renderPlot({
            data <- samples()
            nc <- ifelse(nchain(data) == 1, 1, 2)
            nv <- ceiling(nvar(data)/2)
            mat <- matrix(1:(nv * nc), nrow = nv, ncol = nc,
                          byrow = TRUE)
            par(mar=c(2.6, 2.8, 1.2, 0.5), mgp = c(1.6, 0.6, 0))
            layout(mat)
            gelman.plot(data, auto.layout = FALSE)
        }, height = 800)

        output$gelmanout <- renderPrint({
            gelman.diag(samples())
        })

        # Output for raftery
        output$rafteryout <- renderPrint({
            raftery.diag(samples())
        })

        # Output for heidel
        output$rafteryout <- renderPrint({
            heidel.diag(samples())
        })

        #-------------------------------------------
        # Only used for development
        output$test <- renderPrint({
            # str(input$TRACE_POS)
            # seq(input$TRACE_POS - input$TRACE_SIZE + 1, input$TRACE_POS, by = 1L)
            # str("all" %in% input$VARS)
            # mct_select(object, "")
            # str(samples())
            input$DIAG
            # seq_len(nchain(samples()))
        })
    }
)
