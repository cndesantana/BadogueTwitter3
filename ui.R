<<<<<<< HEAD
dashboardPage(
  dashboardHeader(title = "Badogue - Twitter"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard"),
      textInput("term", "Enter a term", ""),
      sliderInput("count", 
                  label = "Número máximo de tweets para baixar:",
                  min = 0, max = 2000, value = 0),
      selectInput("lang","Select the language",
                  c("English"="en",
                    "Portuguese"="pt",
                    "Spanish"="es"), selected = "pt"),
      actionButton("twitterbtn", "Raspar", class = "btn btn-primary"),
      actionButton(inputId="update", label="Plotar"),
      tags$hr("Baixar dados em formato Excel",br()),
      downloadButton("downloadComments", "Download Comments")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("dashboard",
        fluidRow(
           box(
              width = 6, status = "info", solidHeader = TRUE,
              title = "Comentários",
              plotOutput("commentsPlot", width="100%", height= 600),
              downloadButton("comentariosts","Download")
           )
        ),
        fluidRow(
           box(
              width = 6, status = "info", solidHeader = TRUE,
              title = "Unigramas",
              plotOutput("unigramaPlot", width="100%", height= 600),
              downloadButton("unigrama","Download")
           )
        )
      )
    )
  )
)

=======

# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
shinyUI(fluidPage(
   headerPanel("Twitter Analysis"),
   sidebarPanel( textInput("term", "Enter a term", ""),
                 sliderInput("count",
                             label = "Number of tweets to load:",
                             min = 0, max = 200, value = 0),
                 submitButton(text="Run")),
   mainPanel(
      h4("Algumas análises"),
      plotOutput("wordcl"),
      plotOutput("plotlocal"),
      plotOutput("plotautor"))
))
>>>>>>> f5abfe4798136b0e9e068b2dbd91443eb6bba457
