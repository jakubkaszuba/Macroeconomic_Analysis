source("appStart.R")

# UI ----------------------------------------------------------------------
ui <- shinyUI(
  dashboardPage(
    # HEADER ------------------------------------------------------------------
    dashboardHeader(title = "JECONOMICS"
    ),
    
    # SIDEBAR -----------------------------------------------------------------
    dashboardSidebar(
      sidebarMenu(
        menuItem("Currency pair", tabName = "pair", icon = icon("coins")),
        menuItem("Database", tabName = "yahoo", icon = icon("database"),
                 menuSubItem("Yahoo charts", tabName = "index1", icon = icon("chart-line")),
                 menuSubItem("Nasdaq.com company data", tabName = "index2", icon = icon("bar-chart"))),
        menuItem("DJI (1885-2019)", tabName = "djiMain", icon = icon("bullhorn"),
                 # menuSubItem("Type plot I", tabName = "dji", icon = icon("line-chart")),
                 menuSubItem("Dow Jones Industry", tabName = "dji2", icon = icon("line-chart"))),
        menuItem("Fred.com macro data", tabName = "macrodataMain", icon = icon("balance-scale"),
                 menuSubItem("Basic information", tabName = "macrodata", icon = icon("recycle")),
                 menuSubItem("U.S. Recession Probabilities", tabName = "rec", icon = icon("recycle")))
      )
    ),
    # tags$head(tags$link(rel = "shortcut icon", href = "https://img.icons8.com/ios/24/4a90e2/financial-growth-analysis.png")),
    # BODY --------------------------------------------------------------------
    dashboardBody(tags$head(includeScript("google-analytics.html")),
                  tags$head(tags$link(rel = "shortcut icon", href = "https://img.icons8.com/ios/24/4a90e2/financial-growth-analysis.png")),
                  theme_blue_gradient,
                  tabItems(
                    
                    tabItem(tabName = "index1",
                            fluidRow(
                              box(background = "green" , collapsible = TRUE, width = 12,
                                  fluidRow(
                                    column(4,textInput("symb", "Symbol", "BTC-USD")),
                                    column(4,dateRangeInput("dates",
                                                            "Date range",
                                                            start = "2017-08-01",
                                                            end = as.character(Sys.Date()))),
                                    column(4,checkboxInput("log", "Log scale",
                                                           value = FALSE))
                                  )
                              ),
                              box(title = "Results",
                                  collapsible = TRUE, status = "success",
                                  width = 12, plotOutput("plot3", width = "100%", height = "1200px") %>% withSpinner())
                            )
                    ),
                    
                    tabItem(tabName ="index2",
                            fluidRow(
                              box(title = "Choose sector to see industry details.", solidHeader = TRUE,
                                  background = "teal", collapsible = TRUE, width = 12,
                                  fluidRow(
                                    column(4,uiOutput("sector")),
                                    column(4,uiOutput("indust"))
                                  )
                              )
                            ),
                            box(title = "Sector Market Capitalization", solidHeader = TRUE, 
                                collapsible = TRUE, status = "success",
                              width = 12, plotlyOutput("plotlySector", height = "350px") %>% withSpinner()
                            ),
                            box(title = "Results",
                                collapsible = TRUE, status = "success",
                                width = 12, DT::dataTableOutput('results') %>% withSpinner()
                            ),
                            box(title = "Industry Market Capitalization", solidHeader = TRUE, 
                                collapsible = TRUE, status = "success",
                                width = 12, plotlyOutput("plotlyIndustry", height = "500px") %>% withSpinner()
                            )
                    ),
                    
                    tabItem(tabName = "pair",
                            fluidRow(
                              h4("The price of the most popular currency pairs"),
                              DT::dataTableOutput("plot2", width = "100%", height = "auto") %>% withSpinner()
                            )
                    ),
                    
                    # tabItem(tabName = "dji",
                    #         fluidRow(
                    #           box(plotlyOutput("plot1", height = 500) %>% withSpinner(),width = 500),
                    #           checkboxInput("log1", "Log scale", value = FALSE)   
                    #         )
                    # ),
                    
                    tabItem(tabName = "dji2",
                            fluidPage(
                              dygraphOutput("plot4", height = 600) %>% withSpinner(),
                              checkboxInput("log2", "Log scale", value = FALSE)
                            )
                    ),

# Macrodata Items ---------------------------------------------------------
                    tabItem(tabName = "macrodata",
                            fluidRow(
                              box(background = "green" , collapsible = TRUE, width = 12,
                                  fluidRow(
                                    column(4,dateRangeInput("datesMacro",
                                                            "Date range",
                                                            start = "1970-08-01",
                                                            end = as.character(Sys.Date())))
                                  )
                              )
                            ),
                            box(title = "Unemployment rate", solidHeader = TRUE, 
                                collapsible = TRUE, status = "success",
                                width = 12, dygraphOutput("unemp", height = macroHigh) %>% withSpinner()
                            ),
                            box(title = "Effective Federal Funds Rate", solidHeader = TRUE, 
                                collapsible = TRUE, status = "success",
                                width = 12, dygraphOutput("fund", height = macroHigh) %>% withSpinner()
                            ),
                            box(title = "PMI Composite Index", solidHeader = TRUE, 
                                collapsible = TRUE, status = "success",
                                width = 12, dygraphOutput("pmi1", height = macroHigh) %>% withSpinner()
                            ),
                            box(title = "Real Gross Domestic Product", solidHeader = TRUE, 
                                collapsible = TRUE, status = "success",
                                width = 12, dygraphOutput("rgdp", height = macroHigh) %>% withSpinner()
                            ),
                            box(title = "10-Year Treasury Constant Maturity Minus 2-Year Treasury Constant Maturity", solidHeader = TRUE, 
                                collapsible = TRUE, status = "success",
                                width = 12, dygraphOutput("tcm1", height = macroHigh) %>% withSpinner()
                            ),
                            box(title = "10-Year Treasury Constant Maturity Minus 3-Month Treasury Constant Maturity", solidHeader = TRUE, 
                                collapsible = TRUE, status = "success",
                                width = 12, dygraphOutput("tcm2", height = macroHigh) %>% withSpinner()
                            )
                            ),
                    tabItem(tabName = "rec",
                            box(title = "Smoothed U.S. Recession Probabilities", solidHeader = TRUE,
                                collapsible = TRUE, status = "success",
                                width = 12, dygraphOutput("rec1", height = macroHigh) %>% withSpinner()),
                            box(title = "GDP-Based Recession Indicator Index", solidHeader = TRUE,
                                collapsible = TRUE, status = "success",
                                width = 12, dygraphOutput("gdprec1", height = macroHigh) %>% withSpinner()),
                            box(title = "Business Tendency Surveys for Manufacturing: Confidence Indicators: Composite Indicators: European Commission and National Indicators for the United States", solidHeader = TRUE,
                                collapsible = TRUE, status = "success",
                                width = 12, dygraphOutput("btsm", height = macroHigh) %>% withSpinner()))
                  )
    )
  )
)


# SERVER ------------------------------------------------------------------
server <- function(input, output, session) {

# Yahoo data --------------------------------------------------------------
  dataYahoo <- reactive({
    Sys.sleep(1)
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  output$plot3 <- renderPlot({
    Sys.sleep(1)
    chartSeries(dataYahoo(), theme = chartTheme("white"), type = "matchsticks", log.scale = input$log, TA=c(addVo(), addMACD(), addRSI(), addADX(), addROC(), addCCI()))
    addSMA(n = c(50, 100, 200))
  })
  

# NASDAQ, NYSE, AMEX ------------------------------------------------------
  # colnames(NasdaqCo)[which(names(NasdaqCo) == "MarketCap")] <- "MarketCap [$]"
  output$sector = renderUI({
    selectizeInput(inputId = "sect1",
                   label = "Sector:", 
                   choices = as.character(na.omit(unique(NasdaqCo$Sector))),
                   multiple = TRUE)
  })
  datasub <- reactive({
    completeFun(NasdaqCo[NasdaqCo$Sector == input$sect1,], "Symbol")
  })
  
  output$indust = renderUI({
    selectizeInput(inputId = "ind1", 
                   label = "Industry:", 
                   choices = as.character(unique(datasub()[,"Industry"])),
                   multiple = TRUE)
  })
  
  datasub2 <- reactive({
    completeFun(datasub()[datasub()[,"Industry"] == input$ind1, ], "Symbol")
  })
  
  datasub3 <- reactive({DT::datatable({
    if (is.null(input$sect1) && is.null(input$ind1)) {
      NasdaqCo
    } else if (is.null(input$ind1)) {
      datasub()
    } else if (!is.null(input$sect1) && !is.null(input$ind1)) {
      datasub2()
    } else datasub2()
  })
  })
  
  output$results = DT::renderDataTable(datasub3())
  
  # MarketCap Sectors -------------------------------------------------------
  # pa1 <- plot_ly(NasDF, x = ~Sector, y = ~MarketCap, color = ~Sector) %>%
  #   hide_legend() %>%
  #   layout(
  #     xaxis = ax,
  #     yaxis = ax)
  output$plotlySector <- renderPlotly({
    Sys.sleep(1)
    plot_ly(NasDF, x = ~Sector, y = ~MarketCap, color = ~Sector, type = "bar") %>%
      hide_legend() %>%
      layout(
        xaxis = ax,
        yaxis = ax)
  })

# MarketCap Industries ----------------------------------------------------
  Nas2 <- reactive({
    completeFun(datasub()[datasub()$MarketCap > 0,], "MarketCap")
    # plot_ly(datasub() %>%
    #           group_by(Industry) %>%
    #           summarise(MarketCap = sum(MarketCap, na.rm = TRUE)), 
    #         x = ~Industry, y = ~MarketCap, color = ~Industry) %>% 
    #   hide_legend() %>% 
    #   layout(xaxis = ax, yaxis = ax)
  })
  output$plotlyIndustry <- renderPlotly({
    Sys.sleep(1)
    plot_ly(Nas2() %>%
              group_by(Industry) %>%
              summarise(MarketCap = sum(MarketCap)),
            x = ~Industry, y = ~MarketCap, color = ~Industry, type = "bar") %>%
      hide_legend() %>%
      layout(xaxis = ax, yaxis = ax)
    # Nas2()
    # plot_ly(Nas2, x = ~Industry, y = ~MarketCap, color = ~Industry) %>% hide_legend()
  })
  
  
  # Price Currency ----------------------------------------------------------
  output$plot2 <- renderDT({Price_Currencies}, class = 'cell-border stripe', editable = 'cell')


# Dow Jones Type plot 1 ---------------------------------------------------------------
  DJA <- read.csv(file="DJA.csv", header=TRUE, sep=";")
  DJA$Date <- strptime(as.character(DJA$Date), "%m/%d/%Y")
  # DJA$Date <- as.POSIXct(format(DJA$Date, "%Y-%m-%d"))
  # DJA$logDJIA <- log(DJA$DJIA)# #worldbank.org
  dja2 <- read.zoo(DJA, format = "%Y-%m-%d")
  # # 
  # # dja1 <- ggplot(DJA, aes(x = Date, y = DJIA)) + geom_line() + labs(x = "Date", y = "Price [USD]", title = "Dow Jones Industrial") + theme_minimal()
  # 
  # output$plot1 <- renderPlotly({dja1 + 
  #     if(input$log1){scale_y_continuous(trans = "log10")}})
  
  
# Dow Jones Type plot 2 --------------------------------------------------------------
  dja2a <- dygraph(dja2, main = "Dow Jones Industrial", group = "dziadek") %>% dyRangeSelector() %>% dyRoller(rollPeriod = 0) #dyRoller(rollPeriod = 0, showRoller = FALSE)

  output$plot4 <- renderDygraph({
    if(input$log2 == TRUE) {
      dja2a %>% dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = TRUE, logscale = TRUE)} else dja2a
    })
  

# Macrodata plot ----------------------------------------------------------
  subsetMacro <- function(dataset){
    dataset[input$datesMacro[1] <= index(dataset) & input$datesMacro[2] >= index(dataset)]
  }
  # datesMakro1 <- reactive({
  #   as <- paste(input$datesMacro[1],c("/"))
  #   paste(as, input$datesMacro[2])
  # })
  # subsetMacroLoop <- function(dataset, ) {
  #   
  # }
  # Unemployment rate
  UN1 <- reactive({
    subsetMacro(UN)
  })
  # Effective Federal Funds Rate
  FundsFED1 <- reactive({
    subsetMacro(FundsFED)
  })
  # Real Gross Domestic Product
  RGDP1 <- reactive({
    subsetMacro(RGDP)
  })
  TCM11 <- reactive({
    subsetMacro(TCM1)
  })
  TCM22 <- reactive({
    subsetMacro(TCM2)
  })
  PMI1 <- reactive({
    subsetMacro(PMI)
  })
  REC1 <- reactive({
    subsetMacro(REC)
  })
  GDPREC1 <- reactive({
    subsetMacro(GDPREC)
  })
  BTSM1 <- reactive({
    subsetMacro(BTSM)
  })
  output$unemp <- renderDygraph({
    dygraph(UN1(), main = NULL, group = "unemploy") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 0)
  })
  output$fund <- renderDygraph({
    dygraph(FundsFED1(), main = NULL, group = "fundrate") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 0)
  })
  output$pmi1 <- renderDygraph({
    dygraph(PMI1(), main = NULL, group = "pmi1") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 0)
  })
  output$rgdp <- renderDygraph({
    dygraph(RGDP1(), main = NULL, group = "rgdp") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 0)
  })
  output$tcm1 <- renderDygraph({
    dygraph(TCM11(), main = NULL, group = "tcm1") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 0)
  })
  output$tcm2 <- renderDygraph({
    dygraph(TCM22(), main = NULL, group = "tcm2") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 0)
  })
  output$rec1 <- renderDygraph({
    dygraph(REC1(), main = NULL, group = "rec1") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 0)
  })
  output$gdprec1 <- renderDygraph({
    dygraph(GDPREC1(), main = NULL, group = "gdprec1") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 0)
  })
  output$btsm <- renderDygraph({
    dygraph(BTSM1(), main = NULL, group = "btsm") %>%
      dyRangeSelector() %>%
      dyRoller(rollPeriod = 0)
  })
}
shinyApp(ui, server)