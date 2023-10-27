
## Load packages ------------------------------

library(shiny)
library(shinydashboard)
library(shinyBS)
library(rintrojs)
library(shinyWidgets)
library(shinyjs)
library(plotly)
library(tidyverse)
library(stopwords)
library(wordcloud2)
library(tidytext)
library(tidygraph)
library(ggraph)
library(wordcloud)
library(igraph)
library(DT)
library(webr)
library(readxl)

## Read data --------------------------------------
IG_Telco_posts_clean <- read.csv("clean_data/Instagram/IG_Telco_posts.csv")
IG_Telco_comments_clean <- read.csv("clean_data/Instagram/IG_Telco_comments.csv")
FB_Telco_posts_clean <- read.csv("clean_data/Facebook/FB_Telco_posts.csv")
FB_Telco_comments_clean <- read.csv("clean_data/Facebook/FB_Telco_comments.csv")
FB_amaysim <- read_excel("result_for_histogram/Facebook/amaysim_fb_post_hist.xlsx")
FB_optus <- read_excel("result_for_histogram/Facebook/optus_fb_post_hist.xlsx")
FB_telstra <- read_excel("result_for_histogram/Facebook/telstra_fb_post_hist.xlsx")
FB_vodafoneAU <- read_excel("result_for_histogram/Facebook/vodafoneAU_fb_post_hist.xlsx")
IG_amaysim <- read_excel("result_for_histogram/Instagram/amaysim_ig_post_hist.xlsx")
IG_optus <- read_excel("result_for_histogram/Instagram/optus_ig_post_hist.xlsx")
IG_telstra <- read_excel("result_for_histogram/Instagram/telstra_ig_post_hist.xlsx")
IG_vodafoneAU <- read_excel("result_for_histogram/Instagram/vodafoneAU_ig_post_hist.xlsx")

## Data Wangling -----------------------------------------
FB_url <- FB_Telco_posts_clean %>% 
  select(brand, post_url)

IG_url <- IG_Telco_posts_clean %>% 
  select(brand, post_url)

## FB post
FB_Telco_posts_clean <- FB_Telco_posts_clean %>%
  mutate(brand = case_when(brand == "Vodafone Australia" ~ "Vodafone AU",
                           brand == "amaysim" ~ "Amaysim",
                           .default = brand),
         post_date = as.Date(post_date)) %>% 
  pivot_longer(cols = c(reactions, comments, shares),
               names_to = "category",
               values_to = "number") %>% 
  select(brand, category, number, post_date, content)

## IG post
IG_Telco_posts_clean <- IG_Telco_posts_clean %>%
  mutate(brand = case_when(brand == "amaysim" ~ "Amaysim",
                           .default = brand),
         post_date = as.Date(post_date),
         numbers = parse_number(numbers),
         id = row_number()) %>% 
  select(id, brand, numbers, interactions,
         comments, post_date, content = post_caption)

IG_Telco_posts_clean_like_view <- IG_Telco_posts_clean %>% 
  select(id, numbers, interactions) %>% 
  pivot_wider(id_cols = id,
              names_from = interactions,
              values_from = numbers,
              values_fill = 0)

IG_Telco_posts_clean_comment <- IG_Telco_posts_clean %>% 
  select(-numbers, -interactions)

IG_Telco_posts_clean <- IG_Telco_posts_clean_like_view %>% 
  inner_join(IG_Telco_posts_clean_comment,
             by = "id",
             relationship = "many-to-many") %>% 
  pivot_longer(cols = c(views, likes, comments),
               names_to = "category",
               values_to = "number") %>% 
  select(brand, category, number, post_date, content)

## FB comments
FB_Telco_comments_clean = FB_Telco_comments_clean %>% 
  inner_join(FB_url,by = "post_url") %>% 
  mutate(date = as.Date(post_date),
         brand = case_when(brand == "Vodafone Australia" ~ "Vodafone AU",
                           brand == "amaysim" ~ "Amaysim",
                           .default = brand)) %>% 
  select(brand,date,content = comment_content)

## IG comments
IG_Telco_comments_clean <- IG_Telco_comments_clean %>% 
  inner_join(IG_url,
             by = "post_url",
             relationship = "many-to-many") %>% 
  mutate(date = as.Date(date),
         brand = case_when(brand == "amaysim" ~ "Amaysim",
                           .default = brand)) %>% 
  select(brand, date, content)

## Stop words
stopword <- get_stopwords(source = "snowball")

colPalette <- c("#39A8AF", "#FECD03","#0060AE", "#0A8AD2", 
                "#e82f89", "#9b0082","#ef2d1e", "#FF5500")

## FB post token
FB_post_word <- FB_Telco_posts_clean %>%
  select(brand, post_date, content) %>% 
  distinct() %>%
  mutate(content = gsub("[^a-zA-Z|[:blank:]]", "", content)) %>%
  unnest_tokens(output = word,
                input = content,
                token = "words") %>%
  anti_join(stopword, by = "word") %>%
  filter(!str_detect(word, "^[[:digit:]]"))

## IG post token
IG_post_word <- IG_Telco_posts_clean %>%
  select(brand, post_date, content) %>% 
  distinct() %>% 
  mutate(content = gsub("[^a-zA-Z|[:blank:]]", "", content)) %>%
  unnest_tokens(output = word,
                input = content,
                token = "words") %>%
  anti_join(stopword, by = "word") %>%
  filter(!str_detect(word, "^[[:digit:]]"))

## FB comment token
FB_comment_word <- FB_Telco_comments_clean %>%
  select(brand, date, content) %>% 
  distinct() %>%
  unnest_tokens(output = word,
                input = content,
                token = "words") %>%
  anti_join(stopword, by = "word") %>%
  filter(!str_detect(word, "^[[:digit:]]"))

## IG comment token
IG_comment_word <- IG_Telco_comments_clean %>%
  select(brand, date, content) %>% 
  distinct() %>%
  unnest_tokens(output = word,
                input = content,
                token = "words") %>%
  anti_join(stopword, by = "word") %>%
  filter(!str_detect(word, "^[[:digit:]]"))

## FB themes
data.frame(append(FB_amaysim, c(brand = "Amaysim"), after = 0)) -> FB_amaysim
data.frame(append(FB_optus, c(brand = "Optus"), after = 0)) -> FB_optus
data.frame(append(FB_telstra, c(brand = "Telstra"), after = 0)) -> FB_telstra
data.frame(append(FB_vodafoneAU, c(brand = "Vodafone AU"), after = 0)) -> FB_vodafoneAU

FB_col <- rbind(FB_amaysim, FB_optus, FB_telstra, FB_vodafoneAU) %>%
  mutate(Themes = substr(Themes, 9, nchar(Themes)),
         Themes = gsub("_", " ", Themes))

## IG themes
data.frame(append(IG_amaysim, c(brand = "Amaysim"), after = 0)) -> IG_amaysim
data.frame(append(IG_optus, c(brand = "Optus"), after = 0)) -> IG_optus
data.frame(append(IG_telstra, c(brand = "Telstra"), after = 0)) -> IG_telstra
data.frame(append(IG_vodafoneAU, c(brand = "Vodafone AU"), after = 0)) -> IG_vodafoneAU

IG_col <- rbind(IG_amaysim, IG_optus, IG_telstra, IG_vodafoneAU) %>%
  mutate(Themes = substr(Themes, 9, nchar(Themes)),
         Themes = gsub("_", " ", Themes))


## ui--------------------------------------------------

ui <- dashboardPage(
  skin = "black",
  title = "R Shiny",
  
  dashboardHeader(
    title = span("Social Media Analysis"),
    titleWidth = 273
    
  ),
  
## sidebar------------------------------------------------

  dashboardSidebar(
    width = 273,
    sidebarMenu(div(class = "inlay", 
                    style = "height:19px;width:100%;background-color: #ecf0f5;"),
                
                menuItem(
                  text = "Menu Item",
                  tabName = "sources",
                  icon = icon("house")
                  ),
                
                menuItem(
                  text = "Data Sources",
                  icon = icon("address-card"),
                  radioButtons(
                    inputId = "dataSource",
                    label = NULL,
                    choices = list("Facebook" = "Facebook",
                                   "Instagram" = "Instagram"),
                    selected = "Facebook"
                    ),
                  br()
                  ),
                br(),
                
                menuItem(
                  "Date Ranges",
                  icon = icon("calendar"),
                  dateRangeInput(
                    inputId = "dateRanges",
                    label = NULL,
                    start = "2020-03-04",
                    end = "2023-08-15",
                    min = "2020-03-04",
                    max = "2023-08-15"
                    ),
                  br()
                  ),
                br(),
                
                menuItem(
                  "Brands",
                  icon = icon("signal"),
                  pickerInput(
                    inputId = "brandSelect",
                    label = NULL,
                    choices = c("Amaysim", "Optus", "Telstra", "Vodafone AU"),
                    selected = c("Amaysim", "Optus", "Telstra", "Vodafone AU"),
                    options = list(`actions-box` = TRUE, `none-selected-text` = "Please at least select a division"),
                    multiple = T
                    ),
                  br()
                  ),
                br()
                )
    ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "sources",
              tabsetPanel(tabPanel("Post",
                                   
                                   fluidRow(valueBoxOutput(outputId = "value1", width = 4),
                                            valueBoxOutput(outputId = "value2", width = 4),
                                            valueBoxOutput(outputId = "value3", width = 4)),
                                   
                                   fluidRow(box(width = 12,
                                                status = "primary",
                                                plotlyOutput("line", height = "400px")
                                       )
                                   ),
                                   
                                   fluidRow(box(width = 6,
                                                status = "primary",
                                                wordcloud2Output("word", height = "360px")),
                                            box(width = 6,
                                                status = "primary",
                                                plotOutput("sentiment", height = "360px"))),
                                   
                                   fluidRow(box(width = 12,
                                                status = "primary",
                                                plotOutput("col", height = "400px")))
                                   ),
                          
                          tabPanel("Comment",
                                   fluidRow(box(width = 6,
                                                status = "primary",
                                                plotOutput("network", height = "500px")),
                                            box(width = 6,
                                                status = "primary",
                                                plotOutput("wordcloud", height = "500px", width = "100%"),
                                                div(
                                                  style = "position: absolute; right: 0.5em; bottom: 0.5em;",
                                                    actionBttn(
                                                      inputId = "zoom",
                                                      icon = icon("search-plus", class = "opt"),
                                                      style = "fill",
                                                      color = "danger",
                                                      size = "xs"
                                                    )
                                                  )
                                                )
                                            ),
                                   
                                   fluidRow(box(width = 6,
                                                status = "primary",
                                                plotOutput("dounghnut", height = "400px")),
                                            box(width = 6,
                                                status = "primary",
                                                DT::dataTableOutput("table")))
                                   )
                          )
              
              )
      )
    )
        
)


server = function(input, output) {

  line_data = reactive({
    
    if(input$dataSource == "Facebook"){
      
      FB_Telco_posts_clean %>% 
        select(-content) %>% 
        filter(between(post_date, input$dateRanges[1], input$dateRanges[2]),
               brand %in% input$brandSelect)
      } 
    else{
      
      IG_Telco_posts_clean %>% 
        select(-content) %>% 
        filter(between(post_date, input$dateRanges[1], input$dateRanges[2]),
               brand %in% input$brandSelect)
      }
    
  })
  
  
  network_data = reactive({
    
    if(input$dataSource == "Facebook"){
      
      FB_Telco_comments_clean %>% 
        filter(between(date,input$dateRanges[1],input$dateRanges[2]),
               brand %in% input$brandSelect)
      
    } 
    else{
      
      IG_Telco_comments_clean %>% 
        filter(between(date,input$dateRanges[1], input$dateRanges[2]),
               brand %in% input$brandSelect)
      
    }
    
  })
  
  
  post_word = reactive({
    
    if(input$dataSource == "Facebook"){
      
      FB_post_word %>% 
        filter(between(post_date,input$dateRanges[1], input$dateRanges[2]),
               brand %in% input$brandSelect)
      
    } 
    else{
      
      IG_post_word %>% 
        filter(between(post_date,input$dateRanges[1], input$dateRanges[2]),
               brand %in% input$brandSelect)
      
      }
    
    })
  
  
  comment_word = reactive({
    
    if(input$dataSource == "Facebook"){
      
      FB_comment_word %>% 
        filter(between(date, input$dateRanges[1], input$dateRanges[2]),
               brand %in% input$brandSelect)
      } 
    else{
      
      IG_comment_word %>% 
        filter(between(date, input$dateRanges[1], input$dateRanges[2]),
               brand %in% input$brandSelect)
      
      }
    
    })
  
  
  col_data = reactive({
    
    if(input$dataSource == "Facebook"){
      
      FB_col %>% 
        filter(brand %in% input$brandSelect)
    } 
    else{
      
      IG_col %>% 
        filter(brand %in% input$brandSelect)
      
    }
    
  })
  
  
  # valuebox ------------------------------------------
  
  output$value1 = renderValueBox({
    
    values = line_data() %>%
      group_by(category) %>% 
      summarise(number = sum(number)) %>% 
      ungroup()
    
    
    valueBox(value = values$number[2],
             subtitle = tags$p(values$category[2], style = "font-size: 150%;"),
             icon = icon(name = "thumbs-up"),
             color = "purple")
    
  })
  
  output$value2 = renderValueBox({
    
    values = line_data() %>%
      group_by(category) %>% 
      summarise(number = sum(number)) %>% 
      ungroup()
    
    
    valueBox(value = values$number[1],
             subtitle = tags$p(values$category[1], style = "font-size: 150%;"),
             icon = icon(name = "comment"),
             color = "green")
    
  })
  
  output$value3 = renderValueBox({
    
    values = line_data() %>%
      group_by(category) %>% 
      summarise(number = sum(number)) %>% 
      ungroup()
    
    
    valueBox(value = values$number[3],
             subtitle = tags$p(values$category[3], style = "font-size: 150%;"),
             icon = icon(name = "user"),
             color = "yellow")
    
  })
  
  
  # line plot ---------------------------------------------
  
  output$line <- renderPlotly({
    
    line_data() %>%
      ggplot(aes(x = post_date)) +
      geom_line(aes(y = number, color = brand,
                    linetype = category)) +
      geom_point(aes(y = number, color = brand, 
                     shape = category), 
                 size = 2) +
      geom_hline(yintercept = 100, linetype = "dashed", 
                 color = "black", linewidth = 0.7) +
      facet_wrap(~category, scales = "free") +
      labs(title = "Trend Analysis by Brand",
           x = NULL,
           y = "Magnitude",
           color = NULL) +
      scale_color_manual(values = c("Telstra" = "#0060AE", "Optus" = "#39A8AF", 
                                    "Vodafone AU" = "#ef2d1e", "Amaysim" = "#FF5500")) +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(angle = 30,
                                       vjust = 0.5))
    
  })
    
  
  # wordcloud1 -------------------------------------------------
  
  output$word <- renderWordcloud2({
    
    post_word() %>%
      group_by(word) %>%
      summarise(freq = n()) %>%
      top_n(60) %>%
      wordcloud2(color = rep_len(colPalette, nrow(post_word())),
                 size = 0.55,
                 shape = "circle")
    
  })  
  

  # sentiment ---------------------------------
  
  output$sentiment <- renderPlot({
    
    post_word() %>%
      inner_join(get_sentiments("bing")) %>%
      mutate(Year = year(post_date),
             Month = month(post_date),
             Date = str_c(Year, "-",
                          Month, "-15"),
             Date = as.Date(Date)) %>%
      count(brand, index = Date, sentiment) %>%
      pivot_wider(names_from = sentiment,
                  values_from = n,
                  values_fill = 0) %>%
      mutate(sentiment = positive - negative) %>%
      ggplot(aes(x = index,
                 y = sentiment,
                 fill = brand)) +
      geom_col(show.legend = F,
               alpha = 0.7) +
      scale_fill_manual(values = c("#FF5500", "#39A8AF", 
                                   "#0060AE", "#ef2d1e")) +
      facet_wrap(~brand, ncol = 2,
                 scales = "free_y") +
      theme_minimal() +
      labs(x = "Date") +
      theme(text = element_text(size = 16),
            legend.position = "none",
            axis.title.x = element_text(vjust = -1),
            axis.text.x = element_text(angle = 30,
                                       vjust = 0.5))
    
  })

  
  # column ----------------------------------
  
  output$col <- renderPlot({
    
    col_data() %>%
      mutate(Themes = reorder_within(Themes, Count, brand)) %>%
      ggplot(aes(x = Themes, y = Count, fill = brand)) +
      geom_col(show.legend = F,
               alpha = 0.7) +
      coord_flip() +
      facet_wrap(~ brand, scales = "free", nrow = 2) +
      scale_fill_manual(values = c("#FF5500", "#39A8AF", 
                                   "#0060AE", "#ef2d1e")) +
      scale_x_reordered() +
      labs(x = NULL,
           y = NULL) +
      theme_minimal() +
      theme(legend.position = "none",
            text = element_text(size = 16))
    
  })
  
  
  # network ------------------------------------------
  
  output$network <- renderPlot({
    
    Com_bigrams <- network_data() %>%
      mutate(id = row_number()) %>%
      mutate(content = gsub("[^a-zA-Z|[:blank:]]", "", content)) %>%
      unnest_tokens(output = bigram,
                    input = content,
                    token = "ngrams",
                    n = 2) %>%
      filter(!is.na(bigram)) %>%
      separate(bigram, c("word1", "word2"),
               sep = " ") %>%
      filter(!word1 %in% stopword$word) %>%
      filter(!word2 %in% stopword$word) %>%
      count(word1, word2, sort = T)
    
    Bigram_graph <- Com_bigrams %>%
      top_n(40) %>%
      graph_from_data_frame()
    
    set.seed(2017)
    ggraph(Bigram_graph, layout = "fr") +
      geom_edge_link() +
      geom_edge_density(aes(fill = n)) +
      geom_node_point(color = "#D221A2", size = 2.5) +
      geom_node_text(aes(label = name,
                         size = 2),  repel = TRUE) +
      theme_void() + 
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5, size = 16)) +
      ggtitle("Bigram Network")
    
  })
  
  
  # wordcloud2 -------------------------------------------------
  
  output$wordcloud <- renderPlot({
    
    comment_word() %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = T) %>%
      reshape2::acast(word ~ sentiment, 
                      value.var = "n",
                      fill = 0) %>%
      comparison.cloud(colors = c("#c30101", "#01C301"),
                       max.words = 150,
                       scale = c(3,.5),
                       title.size = NULL,
                       random.order = FALSE)
  })
  
  observeEvent((input$zoom), {
    
    showModal(modalDialog(
      renderPlot({
        comment_word() %>%
          inner_join(get_sentiments("bing")) %>%
          count(word, sentiment, sort = T) %>%
          reshape2::acast(word ~ sentiment, 
                          value.var = "n",
                          fill = 0) %>%
          comparison.cloud(colors = c("#c30101", "#01C301"),
                           max.words = 150,
                           scale = c(3,.5),
                           title.size = NULL,
                           random.order = FALSE)
      }, height = 900),
      easyClose = T,
      size = "l",
      footer = NULL
      
    ))
  })
  
  
  # dounghnut -------------------------------------------
  
  output$dounghnut <- renderPlot({
    
    comment_word() %>%
      inner_join(get_sentiments("nrc")) %>%
      count(brand, sentiment) %>%
      PieDonut(aes(sentiment, count = n),
               labelposition = 1,
               r0 = 0.82)
    
  })
  
  
  # tf-idf table ----------------------------------------
  
  output$table <- DT::renderDataTable({
    
    comment_word() %>%
      anti_join(stopword) %>%
      inner_join(get_sentiments("bing")) %>%
      count(brand, word, sort = T) %>%
      bind_tf_idf(term = word,
                  document = brand,
                  n = n) %>%
      arrange(desc(tf_idf)) %>%
      top_n(30) %>%
      mutate_at(vars(tf, idf, tf_idf), funs(round(., 5)))
    
    
  }, options = list(aLengthMenu = c(8, 15, 30), iDisplayLength = 8))
  

}


shinyApp(ui, server)

