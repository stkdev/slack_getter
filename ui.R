
library("shiny")

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Slackからログとってくるツール"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        "範囲選択",
        start = format(Sys.Date()-1, "%Y-%m-%d"),
        end = format(Sys.Date(), "%Y-%m-%d")
      ),
      textInput('refine',"絞込ワード"),
      checkboxInput('chk_custom', "仮：カスタムフィルタ", value = T),
      hr(),
      tags$p("↓接続先設定↓"),
      hr(),
      uiOutput('tokenOut'),
      uiOutput('channelOut'),
      uiOutput('urlOut')
    ),
    
    # MainPanel
    mainPanel(
       tabsetPanel(
         tabPanel("Plot",
                  fluidRow(
                    column(12, h3("絞り込み")),
                    column(6, tableOutput("table_filterd")),
                    column(6, plotOutput("graph_filterd")),
                    column(12, h3("全体")),
                    column(6, tableOutput("table_all")),
                    column(6, plotOutput("graph_all"))
                  )
         ),
         tabPanel("List", tableOutput("list")),
         tabPanel("CSV", verbatimTextOutput("csv"))
       )
    )
  )
))
