library(shinydashboard)
library(dygraphs)
library(bubbles)

dashboardPage(
  skin="yellow",
  dashboardHeader(
    title="ehanlin platform Activations Data",
    titleWidth = 450
  ),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Plot", icon = icon("line-chart"),
               menuSubItem("activations line chart", tabName = "plotline", icon = icon("line-chart")),
               menuSubItem("activations bubble", tabName = "plotbubble", icon = icon("bar-chart"))
      ),
      menuItem("Search", icon = icon("search"),
               menuSubItem("查詢用戶開通資料", tabName = "userActivations", icon = icon("table"))
      )

    ),
    hr(),
    conditionalPanel("input.tabs=='plotline' | input.tabs=='plotbubble'",
                     dateRangeInput("Dates", label = h3("Date range"),start=Sys.Date()-30,end=Sys.Date(),min="2014-07-29",language = "zh-TW")
    ),
    conditionalPanel("input.tabs=='userActivations'",
                     textInput("inputUserMailID", "Input user email or ID", ""),
                     radioButtons("userchoice", "select email or ID",
                                  choices = c("email","ID"),
                                  inline=T),
                     div(
                       actionButton('activationsRun','search',icon = icon("search")),
                       style = 'text-align:center; '
                     )
    )
  ),
  dashboardBody(
    #boxes to be put in a row (or column)
    tabItems(
      tabItem(tabName = "plotline",
              fluidRow(
                valueBoxOutput("trialBox"),
                valueBoxOutput("onlineBox"),
                valueBoxOutput("keyBox")
              ),
              
              fluidRow(
                box(title="ehanlin 個人版 每日開通數",
                    status="primary",solidHeader = TRUE,dygraphOutput("dygraph"), height=480, width=12)
              )        
      ),
      
      tabItem(tabName = "plotbubble",
              fluidRow(
                valueBoxOutput("trialProductSetBox"),
                valueBoxOutput("onlineProductSetBox"),
                valueBoxOutput("keyProductSetBox")
              ),
              
              fluidRow(
                box(
                  width = 8, status = "info", solidHeader = TRUE,
                  title = "ehanlin 個人版 產品開通數",
                  bubblesOutput("bubblePlot", width = "100%", height = 600)
                ),
                box(
                  width = 4, status = "info",
                  title = "產品開通表",
                  tableOutput("productSetTable")
                )
              )        
      ),
      
      tabItem(tabName = "ui",
              box( width = NULL, status = "primary", solidHeader = TRUE, title="ui.R",
                   pre(includeText("ui.R"))
              )    
      ),
      
      tabItem(tabName = "server",
              box( width = NULL, status = "primary", solidHeader = TRUE, title="server.R",
                   pre(includeText("server.R"))
              )    
      ),
      
      tabItem(tabName = "userActivations",
              verbatimTextOutput('userActivationsText'),
              div(dataTableOutput('userActivationsTable'))    
      )
      
    )
  )
)
