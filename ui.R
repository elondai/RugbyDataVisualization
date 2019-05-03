library(shiny)
library(shinydashboard)

dashboardPage(
  dashboardHeader(title = "Auckland Rugby"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", icon = icon("dashboard"),
        menuSubItem("Defence", tabName = "defence", icon = icon("shield-alt")),
        menuSubItem("Carries", tabName = "carries", icon = icon("football-ball")),
        menuSubItem("Breakdown", tabName = "breakdown", icon = icon("stop-circle"))),
      menuItem("Athletes", tabName = "athlete", icon = icon("users")),
      menuItem("Analytics", icon = icon("chart-bar"),
        menuSubItem("Correlation-Defence", tabName = "CorDefence", icon = icon("shield-alt")),
        menuSubItem("Correlation-Carries", tabName = "CorCarries", icon = icon("football-ball")),
        menuSubItem("Correlation-Breakdown", tabName = "CorBreakdown", icon = icon("stop-circle"))),
      menuItem("Rawdata", icon = icon("database"),
               menuSubItem("DefenceData", tabName = "rawdata_defence", icon = icon("table")),
               menuSubItem("BreakdownData", tabName = "rawdata_breakdown", icon = icon("table")),
               menuSubItem("CarriesData", tabName = "rawdata_carries", icon = icon("table")),
               menuSubItem("ProfileData", tabName = "rawdata_profile", icon = icon("table")))
    ),
    tags$div(a(href = 'http://www.aucklandrugby.co.nz/',
              img(src = 'logo.png',
                  title = "Auckland Rugby", height = "60px"),
              style = "position:absolute; bottom:11px; left:11px"))
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "defence",
              fluidRow(
                box(title = "Controls for Defence", status = "warning", solidHeader = T, width = 12,
                    column(6,
                           selectInput("sel_defence", "Defence", colnames(df.defence)[-(1:3)])
                    ),
                    column(6,
                           checkboxInput("chk_defence", "Per Minute", TRUE)
                    )
                )
              ),
              fluidRow(
                box(title = "Defence - Ranking", status = "primary", solidHeader = T, width = 6, plotOutput("DefenceRanking", height="550px")),
                box(title = "Defence - Position Based", status = "primary", solidHeader = T, width = 6, plotOutput("DefencePosition", height="550px"))
              )
      ),
      tabItem(tabName = "carries",
              fluidRow(
                box(title = "Controls for Carries", status = "warning", solidHeader = T, width = 12,
                    column(6,
                           selectInput("sel_carries", "Carries", colnames(df.carries)[-(1:4)])
                    ),
                    column(6,
                           checkboxInput("chk_carries", "Per Minute", TRUE)
                    )
                )
              ),
              fluidRow(
                box(title = "Carries - Ranking", status = "primary", solidHeader = T, width = 6, plotOutput("CarriesRanking", height="550px")),
                box(title = "Carries - Position Based", status = "primary", solidHeader = T, width = 6, plotOutput("CarriesPosition", height="550px"))
              )
      ),
      tabItem(tabName = "breakdown",
              fluidRow(
                box(title = "Controls for Breakdown", status = "warning", solidHeader = T, width = 12,
                    column(6,
                           selectInput("sel_breakdown", "Breakdown", colnames(df.breakdown)[-(1:3)])
                    ),
                    column(6,
                           checkboxInput("chk_breakdown", "Per Minute", TRUE)
                    )
                )
              ),
              fluidRow(
                box(title = "Breakdown - Ranking", status = "primary", solidHeader = T, width = 6, plotOutput("BreakdownRanking", height="550px")),
                box(title = "Breakdown - Position Based", status = "primary", solidHeader = T, width = 6, plotOutput("BreakdownPosition", height="550px"))
              )
      ),
      tabItem(tabName = "athlete",
              fluidRow(
                box(title = "Select A Player", status = "warning", solidHeader = T, width = 12, 
                    column(6, 
                           selectInput("sel_player", "Player", sort(df.profile$Player)),
                           dateRangeInput('dateRange',label = "DateRange: ",
                                          start="2018-01-01", end="2020-12-31")
                    ),
                    column(4,
                           infoBox("Position", textOutput("Player_Pos"), width = 12, icon = icon("map-marked-alt"))
                    )
                )
              ),
              fluidRow(
                box(title = "Anthropometric", status = "primary", solidHeader = T, width = 6, plotOutput("Anthropometric")),
                box(title = "YoYo Bronco", status = "primary", solidHeader = T, width = 6, plotOutput("YoYo")),
                box(title = "Upper Strength", status = "primary", solidHeader = T, width = 6, plotOutput("Strength_Upper")),
                box(title = "Lower Strength", status = "primary", solidHeader = T, width = 6, plotOutput("Strength_Lower")),
                box(title = "Power", status = "primary", solidHeader = T, width = 6, plotOutput("Power")),
                box(title = "Speed", status = "primary", solidHeader = T, width = 6, plotOutput("Speed"))
              )
      ),
      tabItem(tabName = "CorDefence",
              fluidRow(
                box(title = "Select A Position", status = "warning", solidHeader = T, width = 12, 
                    column(6, 
                           selectInput("sel_positon_cordef", "Position", c("ALL", unique(df.profile$Position)))
                    )
                )
              ),
              fluidRow(
                box(title = "Correlation-Defence", status = "primary", solidHeader = T, width = 12, plotOutput("CorDefence", height="500px"))
              )
      ),
      tabItem(tabName = "CorCarries",
              fluidRow(
                box(title = "Select A Position", status = "warning", solidHeader = T, width = 12, 
                    column(6, 
                           selectInput("sel_positon_corcar", "Position", c("ALL", unique(df.profile$Position)))
                    )
                )
              ),
              fluidRow(
                box(title = "Correlation-Carries", status = "primary", solidHeader = T, width = 12, plotOutput("CorCarries", height="500px"))
              )
      ),
      tabItem(tabName = "CorBreakdown",
              fluidRow(
                box(title = "Select A Position", status = "warning", solidHeader = T, width = 12, 
                    column(6, 
                           selectInput("sel_positon_corbrk", "Position", c("ALL", unique(df.profile$Position)))
                    )
                )
              ),
              fluidRow(
                box(title = "Correlation-Breakdown", status = "primary", solidHeader = T, width = 12, plotOutput("CorBreakdown", height="500px"))
              )
      ),
      tabItem(tabName = "rawdata_defence",
              fluidPage(
                titlePanel("Defence DataTable"),
                fluidRow(
                  column(12,
                    dataTableOutput(outputId = "DefenceTable"))
                )     
              )
      ),
      tabItem(tabName = "rawdata_breakdown",
              fluidPage(
                titlePanel("Breakdown DataTable"),
                fluidRow(
                  column(12,
                         dataTableOutput(outputId = "BreakdownTable"))
                )     
              )
      ),
      tabItem(tabName = "rawdata_carries",
              fluidPage(
                titlePanel("Carries DataTable"),
                fluidRow(
                  column(12,
                         dataTableOutput(outputId = "CarriesTable"))
                )     
              )
      ),
      tabItem(tabName = "rawdata_profile",
              fluidPage(
                titlePanel("Profile DataTable"),
                fluidRow(
                  column(12,
                         dataTableOutput(outputId = "ProfileTable"))
                )     
              )
      )
    )
  )
)
