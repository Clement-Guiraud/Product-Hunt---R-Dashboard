library(tidyverse)
library(tidytext)
library(shiny)
library(shinydashboard)
library(DT)
library(naniar)
library(mosaic)
library(lubridate)
library(plotly)
library(stringr)
library(ggpubr)
library(ggplot2)
library(gridExtra)

ui = dashboardPage(                                                  
  dashboardHeader(
    title = "ProductHunt Dashboard"
  ),        
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("About", tabName = "about", icon = icon("list-alt", lib = "font-awesome", text = "null")),
      menuItem("Table overview", tabName = "table", icon = icon("table", lib = "font-awesome", text = "null")),
      #menuItem("Global product view", tabName = "overview", icon = icon("table", lib = "font-awesome", text = "null")),
      menuItem("Pricing Type", tabName = "plot", icon = icon("images", lib = "font-awesome", text = "null")),
      menuItem("Topics", tabName = "topic", icon = icon("images", lib = "font-awesome", text = "null")),
      menuItem("Taglines analysis", tabName = "taglines", icon = icon("list-alt", lib = "font-awesome", text = "null")),
      menuItem("Inputs", tabName = "inputvalues", icon= icon("keyboard", lib = "font-awesome", text = "null"),
               sliderInput(
                 "Upvotes", h4("Upvotes"),
                 min = 0, max = 6779,
                 value = c(0, 6779), step = 500, sep = ""
                 ),
               sliderInput(
                 "Comments", h4("Comments"),
                 min = 0, max = 1404,
                 value = c(0, 1404), step = 200, sep = ""
               ),
               checkboxGroupInput(
                 "PricingType", h4("Pricing Type"),
                 choices = list("Free" = "Free",
                                "Free Options" = "Free Options",
                                "Payment Required" = "Payment Required"),
                 selected = c("Free","Free Options","Payment Required")
               )
      )
    )
  ),
  
  dashboardBody(
    tabItems(
    tabItem(tabName = "about", 
            h1("About this dashboard"),
            h4("This dashboard aims at analyzing the trends from", tags$a(href="https://www.producthunt.com/products?order=best_rated&period=2023-3", "ProductHunt"), "in 2022."),
            h4("The data has been gathered on", tags$a(href="https://www.kaggle.com/datasets/undefinenull/product-hunt?select=2022.csv.", "Kaggle"), "."),
            h4("ProductHunt is a place for product-loving enthusiasts to share and geek out about the latest mobile apps, websites, hardware projects, and tech creations."),
            h4("It was created in 2013 by Ryan Hoover, who had already launched several businesses."), 
            h4("ProductHunt is where most entrepreneurs launch their MVPs, try to gather infos from their first customers and receive comments to improve their product."),
            h4("As of 2016, according to Hoover,", tags$b("the website has led to the discovery of over 100 million products across 50,000 companies.")), 
            h4("For instance, SnapChat launched more than 70 versions of their app on the platform."), 
            h4(tags$b("This dashboard aims at analyzing the top trends of 2022 on ProductHunt, since it’s a clear indicator of what the tech market has been"), "- and might look like in the future. "),
            h4("."),
            h4("."),
            h4("."),
            h4("."),
            h4("."),
            fluidRow(
              column(12, valueBoxOutput("panel", width = 20))
            )),
    tabItem(tabName ="table", 
            fluidRow(
              column(4,valueBoxOutput("box1", width = 12)),
              column(4,valueBoxOutput("box2", width = 12)),
              column(4,valueBoxOutput("box3", width = 12))
            ),
            DT::dataTableOutput("table")),
    tabItem(tabName ="plot", 
            fluidRow(
              column(6, valueBoxOutput("info1", width = 20)),
              column(6, valueBoxOutput("info2", width = 20))
            ),
            plotOutput("plot2", height = 400),
            plotOutput("plot", height = 400)),
    tabItem(tabName ="taglines",
            fluidRow(
              column(6, valueBoxOutput("tag1", width = 20)),
              column(6, valueBoxOutput("tag2", width = 20)),
            ),
            plotOutput("word_count", height = 300),
            plotOutput("top_word", height = 300),
            plotOutput("mean_upvote_word", height = 300)),
    tabItem(tabName = "topic", 
            plotOutput("top_cat", height = 400),
            plotOutput("best_word_cat", height = 400))
  ))
)

server <- function(input, output){             
  data <- reactive(
    prodh %>% filter(Upvotes >= input$Upvotes[1],
                     Upvotes <= input$Upvotes[2],
                     Comments >= input$Comments[1],
                     Comments <= input$Comments[2],
                     PricingType %in% input$PricingType)
  )
  
  
  output$panel <- renderInfoBox({
    infoBox(
      title = "Click here",
      fill = TRUE,
      color = "orange",
      value = "PRODUCT HUNT",
      href = "https://www.producthunt.com/",
      icon = icon("gears")
    )
  })
  
  output$box1 <- renderInfoBox({
    infoBox(
      title = "Top 2022 Product",
      subtitle = "click here !",
      fill = FALSE,
      color = "yellow",
      value = "Fitmint",
      href = "https://fitmint.io/",
      icon = icon("comments")
      )
  })
  
  output$box2 <- renderInfoBox({
    infoBox(
      title = "Top 2022 Topic",
      fill = FALSE,
      color = "olive",
      value = "Productivity",
      icon = icon("gears")
    )
  })
  
  output$box3 <- renderInfoBox({
    infoBox(
      title = "Most employed word",
      fill = FALSE,
      color = "maroon",
      value = "'AI'",
      icon = icon("comments")
    )
  })
  
  output$info1 <- renderInfoBox({
    infoBox(
      title = "Free App",
      fill = FALSE,
      color = "olive",
      value = "55% of all products",
      icon = icon("comments")
    )
  })
  
  output$info2 <- renderInfoBox({
    infoBox(
      title = "Mean upvotes for Free Options",
      subtitle = "Against 144 for Free and 117 for Payment Required",
      fill = FALSE,
      color = "blue",
      value = "184",
      icon = icon("gears")
    )
  })
  
  
  output$tag1 <- renderInfoBox({
    infoBox(
      title = "Average number of word per TagLines",
      fill = FALSE,
      color = "maroon",
      value = "8",
      icon = icon("comments")
    )
  })
  
  
  output$tag2 <- renderInfoBox({
    infoBox(
      title = "Most efficient words to get Upvotes in TagLines",
      fill = FALSE,
      color = "yellow",
      value = "'Build', 'Code', 'AI' and 'Free'",
      icon = icon("comments")
    )
  })
  
  output$table <- renderDataTable(data())    
  
  output$plot2 = renderPlot({
    abis = prodh %>%
      ggplot(aes(x = PricingType, fill = PricingType)) + geom_bar() + theme_minimal() + scale_fill_manual(values = c("#FF4536", "#FDB42C", "#61BC4D", "#5195D8", "#B064C2"))
    
    print(abis + theme + ggtitle("Pricing Type repartition"))
  })
  
  output$plot <- renderPlot({
    a = prodh %>%
      ggplot(aes(x = Upvotes, y = PricingType, color = PricingType )) + geom_point(alpha = 0.3) + theme_minimal()
    
    print(a + theme + ggtitle("Pricing Type distribution relative to Upvotes"))
  })
  
  output$top_cat = renderPlot({
    b = agg_tbl
    options(scipen = 5)
    
    print(b + theme + ggtitle("Top Categories"))
  })
  
  output$best_word_cat = renderPlot({
    c = topicss
    
    print(c + theme + ggtitle("Most frequent word per category"))
  })
  
  output$word_count = renderPlot({
    d = word_count
    
    print(d + theme + ggtitle("Average word count per taglines"))
  })
  
  output$top_word = renderPlot({
    e = token %>%
      top_n(10) %>%
      ggplot(aes(y = count, x = reorder(word, -count))) +
      geom_col(fill = "#FF4536", alpha = 0.5) +
      xlab("Word") + ylab("Count") +
      theme_minimal()
    
    print(e + theme + ggtitle("Most popular words to launch an app"))
  })
  
  output$mean_upvote_word = renderPlot({
    f = mea_token %>%
      ggplot(aes(y = mean_upvotes, x = word)) + 
      geom_col(fill = "#FDB42C", alpha = 0.5) + 
      xlab("Word") + ylab("mean_upvote") +
      theme_minimal()
    
    print(f + theme + ggtitle("How efficient are each word"))
  })
}

shinyApp(ui = ui, server = server) 