######
## This is a Shiny web application. You can run the application by clicking
## the 'Run App' button above.
##
## Find out more about building applications with Shiny here:
##
##    http://shiny.rstudio.com/
##
##
#########

# install.packages("rtweet", "ggplot2", "dplyr", "tidytext", "skimr")
# install.packages("textdata")
# install.packages('shiny', 'shinydashboard','shinydashboardPlus')

# library(rtweet)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(textdata)
library(ggplot2)
library(dplyr)
library(tidytext)
library(readr)
library(dplyr)
library(stringr)
library(skimr)
library(wordcloud)
library(RColorBrewer)

## Source Sentiment analysis script 
source("../sentiment_analysis_draft_script.R")
# source('../twitter_script_TDI.R')


ui <- dashboardPage(skin = "blue",

## header                     
                    dashboardHeader(title = "Generate Sentiment Score of online text about a Product or Company", titleWidth = 700  
                                    ),
## Sidebar - need to search term
                    dashboardSidebar(
                      sidebarMenu(
                        sidebarMenuOutput("menu"),
                        sidebarSearchForm(
                          textId = "searchbar",
                          buttonId = "search_box",
                          label = "Search")
                      )
                      
                    ),

##Three items on the shiny app page: score of most recent search, graph of scores next to previous searches, 
                    dashboardBody(
                      fluidPage(
                      fluidRow(
                        infoBox(title = "Sentiment score for Searched Product",
                                value = 1.1, width = "50%")),
                      fluidRow(
                        box(title = "Mean Sentiment Score of product(s)",
                            plotOutput("distPlot"), width = "100%") #, height = "100%")
                            #)
                      ),
                      
                      fluidRow(  
                      box(title = "Most frequently used words about product", 
                          plotOutput("wordcloud"), width = "100%")
                      )
                     )
                    )
                  )
   


# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem(text = "Type product or company name")
    )
    })
  
   output$distPlot <- renderPlot({

     hist_plot_kindle

         })
   
   output$wordcloud <- renderPlot({
     
     set.seed(100000)
     wordcloud(words = count_word_cons_reviews$word,
               freq = count_word_cons_reviews$count,
               min.freq = 1000, max.words=80, scale = c(3, 0.2),
               random.order=F, rot.per=0.35, colors=brewer.pal(8, "Dark2"),
               main = "Hello")
     
   })
   
   # output$wordcloud <- renderPlot({
   #   
   #   wordcloud_kindle
     
   }
   


# Run the application 

shinyApp(ui = ui, server = server)

