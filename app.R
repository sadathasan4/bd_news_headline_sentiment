library(shiny)
library(shinydashboard)
library(reactable)
library(tidyRSS)
library(tidyverse)
library(Rcpp)
library(sentimentr)
library(plotly)
library(tidytext)
library(yarrr)
library(ggrepel)
library(igraph)
library(glue)
library(networkD3)
library(magrittr)
library(cowplot)
library(wordcloud2)
library(tm)
library(ngram)
library(textdata)


ui <- dashboardPage(
  
  dashboardHeader(
    title = "News Analyzer - 880",
    dropdownMenu(type = "message",
                 messageItem(
                   from = "Sadat-Hasan",
                   message = "Welcome to News Analyzer - 880"
                 ),
                 messageItem(
                   from = "sadat.shadow@gmail.com",
                   message = "Any queries?",
                   icon = icon("question")
                 )
    ),
    
    dropdownMenu(type = "notifications",
                 notificationItem(
                   text = "You must reload news before analyzing.",
                   icon("exclamation-triangle")
                 )
      
    ),
    dropdownMenu(type = "tasks", badgeStatus = "success",
                 taskItem(value = 100, color = "green",
                          "App Loaded successfully")
    )
    
    ),
  
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dasboard",tabName = "dashboard", icon = icon("dashboard")),
      menuItem("News Explorer",tabName = "explore", icon = icon("th")),
      menuItem("Sentiment Analytics",tabName = "sentiment", icon = icon("th")),
      menuItem("Topic Network", tabName = "network", icon = icon("th"))
    )
    
    
  ),
  
  dashboardBody(
    
    tabItems(
      
      tabItem(tabName = "dashboard",
              h1("Welcome to News Analyzer - 880."),
              br(),
              strong("Core Features:"),
              p("- gathers and Explore last 24 hours important news from popular English news portals of Bangladesh"),
              p("- Search and query news"),
              p("- Download news in csv or excel format"),
              p("- Get updated news every moment. Press Load/Reload button"),
              p("- News Analyzer - Alpha is a completely Reactive application. All elements are updated with soruce."),
              br(),
              strong("Analytics Features:"),
              p("- Tabular view of the latest news feed with download option"),
              p("- Sentiment Analysis: News Portal wise sentiment Distribution, Positive, Negative and Neutral sentiment statistics"),
              p("- NRC emotion detection: Anger, Anticipation, Disgst, Fear, Joy, Sadness, Surprise, Trust"),
              br(),
              strong("Text Network Analysis Features:"),
              p("- Bigram, Skipgram weight Distribution"),
              p("- Degree, Closeness and Betweenness centrality"),
              p("- Word Network Graph Visualization and Analysis with Layout, Centrality and clustering features in D3 object"),
              br(),
              strong("Work in Progress:"),
              p("- Database Integration"),
              p("- Historic Data Query, Large data analysis")
              ),
      
      tabItem(tabName = "explore",
              
              fluidPage(
                
                fluidRow(
                  
                  actionButton("reload", label = "Load / Reload News"),
                  helpText(em("Note:Refresh to load last 24 hours news"))
                  
                ),
                
                fluidRow(
                  
                  DT::dataTableOutput(outputId = "dt_news")
      
                )
                
                
              )),
      
      tabItem(tabName = "sentiment",
              fluidPage(
                
                fluidRow(
                  
                  column(width = 6,
                    plotOutput("bxp")),
                  
                  column(width = 6,
                    plotlyOutput("brp"))
                  
                ),

                fluidRow(
                  DT::dataTableOutput(outputId = "td_summary")),
                
                fluidRow(

                  column(width = 8,
                      plotOutput("plot_emo")),
                  
                  column(width = 4,
                      plotlyOutput("plot_nrc"))
                  
                )
                
                
                
                )
              ),

      tabItem(tabName = "network",
              
              
                fluidPage(
                  forceNetworkOutput(outputId = "net", height = "1000px"),
                
                # Shiny versions prior to 0.11 should use class = "modal" instead.
                absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                              draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                              width = 330, height = "auto",
                              
                              h3("Degrees"),
                              
                              plotOutput("plot_deg", height = "200px"),
                              plotOutput("plot_clo", height = "200px"),
                              plotOutput("plot_bet", height = "200px")
                              )
                )
              )
      
    )
    
  )
  
  
)




server <- function(input, output, session) {
  
  
  stopwords.df <- tibble(
    word = c(stopwords(kind = 'en'))
  )
  
  #News Aggregation Function
  
  load_news <- function() {
    
    gds_tidy <- tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Fwww.thedailystar.net%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gpa_tidy <- tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Fen.prothomalo.com%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gdt_tidy <- tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Fwww.dhakatribune.com%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gna_tidy <- tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Fwww.newagebd.net%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gun_tidy <- tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Funb.com.bd%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gbn_tidy <- tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Fbdnews24.com%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gfe_tidy <- tidyfeed("https://news.google.com/rss/search?q=site%3Athefinancialexpress.com.bd%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gjn_tidy <- tidyfeed("https://news.google.com/rss/search?q=site%3Ajagonews24.com%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    
    
    gcomb <- bind_rows(gds_tidy, gpa_tidy, gdt_tidy, gna_tidy, gun_tidy,gbn_tidy,gfe_tidy,gjn_tidy)
    
    gcomb_rss <- gcomb %>%
      select(-item_category) %>%
      #mutate(news_url = str_extract(feed_title, "(?<=//)[^\\s/:]+")) %>%
      #mutate(news_url = str_replace(news_url, "//","")) %>%
      #mutate(news_url = str_replace(news_url, "www.", "")) %>%
      #mutate(news_url = str_extract(news_url, "[^/]+")) %>%
      mutate(news_url = str_extract(item_title,'-[^-]+$')) %>%
      mutate(news_url = sub("- ","", news_url)) %>%
      mutate(item_title = sub('-[^-]+$', '', item_title)) %>%
      mutate(item_title = sub('-[^-]+$', '', item_title))
    
    gcomb_rss <- gcomb_rss %>%
      select(item_title,item_link,item_pub_date,news_url)
    
    gcomb_rss

  }
  
  
  #News Aggregation
  
  newsx <- eventReactive(input$reload,{
    load_news()
  })
  

  output$dt_news <- DT::renderDataTable(
    DT::datatable(newsx(),
                  colnames = c("News Title","News Source URL","Publish Time","News URL"),
                  caption = "Last 24 hours news from popular sources",
                  options = list(
                    paging = TRUE,    ## paginate the output
                    pageLength = 15,  ## number of rows to output for each page
                    lengthMenu = list(c(15, -1), c("15", "All")),
                    scrollX = TRUE,   ## enable scrolling on X axis
                    scrollY = TRUE,   ## enable scrolling on Y axis
                    autoWidth = FALSE, ## use smart column width handling
                    server = FALSE,   ## use client-side processing
                    dom = 'Bfrtip',
                    buttons = c('csv', 'excel')
                  ),
                  extensions = 'Buttons',
                  selection = 'single', ## enable selection of a single row
                  filter = 'bottom',              ## include column filters at the bottom
                  rownames = FALSE                ## don't show row numbers/names
                  )
    
  )
  
  #Sentiment Analysis Function
  
  news_senti <- function(gcomb_rss) {
    
    gcb <- gcomb_rss
    
    gcomb_senti1 <- sentiment_by(gcomb_rss$item_title)
    
    gcomb_senti2 <- bind_cols(gcb, gcomb_senti1)
    
    gcomb_senti3 <- gcomb_senti2 %>%
      mutate(polarity_level = ifelse(ave_sentiment > 0, "Positive", ifelse(ave_sentiment < 0, "Negative", ifelse(ave_sentiment == 0, "Neutral", "no"))))
    
    gcomb_senti3
    
  }
  
  gcomb_senti3 <- reactive({news_senti(newsx())})
  
  #Sentiment Analysis boxplot

  
  gcomb_box <- reactive({
    
    gcomb_senti3() %>%
      ggplot() + 
      geom_boxplot(aes(y=news_url, x = ave_sentiment))
    
  })
  

  output$bxp <- renderPlot({gcomb_box()})
  
  #Sentiment Analysis barplot
  
  gcomb_col <- reactive({
    
    gcomb_senti3() %>%
      count(news_url,polarity_level) %>%
      ggplot() + geom_col(aes(y = news_url, x = n, fill = polarity_level)) + theme_minimal()
    
  })
  
  

  output$brp <- renderPlotly({gcomb_col()})
  
  
  #Tidy Data
  
  gtd <- function(gcomb_senti3){
    
    gcomb_tidy <- gcomb_senti3 %>%
      mutate(item_title_v2 = item_title) %>%
      unnest_tokens(word,item_title) %>%
      anti_join(stop_words)
    
    gcomb_tidy
    
  }
  
  
  
  gcomb_tidy <- reactive({
    
    gtd(gcomb_senti3())
 
  })
  

  #Tidy Data Summary Table
  
  td_summary <- function(gcomb_tidy) {
    
    word_summary <- gcomb_tidy %>%
      mutate(news_url = ifelse(is.na(news_url),"NONE", news_url)) %>%
      group_by(news_url, item_title_v2) %>%
      mutate(word_count = n_distinct(word)) %>%
      select(item_title_v2, Newspaper = news_url, Polarity = polarity_level, word_count) %>%
      distinct() %>% #To obtain one record per song
      ungroup()
    
    word_summary
    
  }
  
  
  word_summary <- reactive({
    
    td_summary(gcomb_tidy())
    
  })

  
  output$td_summary <- DT::renderDataTable(
    DT::datatable(word_summary(),
                  colnames = c("News Title","News Portal","Polarity","Word Count"),
                  caption = "News Polarity",
                  options = list(
                    paging = TRUE,    ## paginate the output
                    pageLength = 15,  ## number of rows to output for each page
                    lengthMenu = list(c(15, -1), c("15", "All")),
                    scrollX = TRUE,   ## enable scrolling on X axis
                    scrollY = TRUE,   ## enable scrolling on Y axis
                    autoWidth = FALSE, ## use smart column width handling
                    server = FALSE,   ## use client-side processing
                    dom = 'Bfrtip',
                    buttons = c('csv', 'excel')
                  ),
                  extensions = 'Buttons',
                  selection = 'single', ## enable selection of a single row
                  filter = 'top',              ## include column filters at the bottom
                  rownames = FALSE                ## don't show row numbers/names
    )
    
  )
  
  #NRC Emotion Plot
  
  nrc <- function(gcomb_tidy) {
    
    gcomb_nrc <- gcomb_tidy %>%
      inner_join(get_sentiments("nrc"))
 
  }
  
  
  gcomb_nrc <- reactive({
    
    nrc(gcomb_tidy())
    
  })
  

  theme_lyrics <- function(aticks = element_blank(),
                           pgminor = element_blank(),
                           lt = element_blank(),
                           lp = "none")
  {
    theme(plot.title = element_text(hjust = 0.5), #Center the title
          axis.ticks = aticks, #Set axis ticks to on or off
          panel.grid.minor = pgminor, #Turn the minor grid lines on or off
          legend.title = lt, #Turn the legend title on or off
          legend.position = lp) #Turn the legend on or off
  }
  
  
  emo_plot <- function(gcomb_nrc) {
    
    plot_words <- gcomb_nrc %>%
      group_by(sentiment) %>%
      count(word, sort = TRUE) %>%
      arrange(desc(n)) %>%
      slice(seq_len(8)) %>% #consider top_n() from dplyr also
      ungroup()
    
    word_plots <- plot_words %>%
      #Set `y = 1` to just plot one variable and use word as the label
      ggplot(aes(word, 1, label = word, fill = sentiment )) +
      #You want the words, not the points
      geom_point(color = "transparent") +
      #Make sure the labels don't overlap
      geom_label_repel(force = 1,nudge_y = .5,  
                       direction = "y",
                       box.padding = 0.04,
                       segment.color = "transparent",
                       size = 5) +
      facet_grid(~sentiment) +
      theme_lyrics() +
      theme(axis.text.y = element_blank(), axis.text.x = element_blank(),
            axis.title.x = element_text(size = 9),
            panel.grid = element_blank(), panel.background = element_blank(),
            panel.border = element_rect("lightgray", fill = NA),
            strip.text.x = element_text(size = 9)) +
      xlab(NULL) + ylab(NULL) +
      ggtitle("News Portal Emotion wise Word Analysis") +
      coord_flip()
    
    word_plots
    
  }
  
  emotion_plot <- reactive({
    
    emo_plot(gcomb_nrc())
    
  })
  
  
  output$plot_emo <- renderPlot(
    {emotion_plot()}
  )
  
  
  nrc_pl <- function(gcomb_nrc) {
    
    nrc_plot <- gcomb_nrc %>%
      group_by(sentiment) %>%
      summarise(word_count = n()) %>%
      ungroup() %>%
      mutate(sentiment = reorder(sentiment, word_count)) %>%
      #Use `fill = -word_count` to make the larger bars darker
      ggplot(aes(sentiment, word_count, fill = -word_count)) +
      geom_col() +
      guides(fill = FALSE) + #Turn off the legend
      theme_lyrics() +
      labs(x = NULL, y = "Word Count") +
      #scale_y_continuous(limits = c(0, 500)) + #Hard code the axis limit
      ggtitle("News NRC Sentiment") +
      coord_flip()
    
    nrc_plot
    
    
  }
  
  plot_nrc <- reactive({
    
    nrc_pl(gcomb_nrc())
    
  })
  
  
  output$plot_nrc <- renderPlotly(
    {plot_nrc()})

  
  #Network Analysis
  
  # skipgram distribution
  
  graph_distr <- function(gcomb_senti3) {
    
    skip.window <- 2
    
    skip.gram.words <- gcomb_senti3 %>% 
      unnest_tokens(
        input = item_title, 
        output = skipgram, 
        token = 'skip_ngrams', 
        n = skip.window
      ) %>% 
      filter(! is.na(skipgram))
    
    
    
    skip.gram.words$num_words <- skip.gram.words$skipgram %>% 
      map_int(.f = ~ ngram::wordcount(.x))
    
    skip.gram.words %<>% filter(num_words == 2) %>% select(- num_words)
    
    skip.gram.words %<>% 
      separate(col = skipgram, into = c('word1', 'word2'), sep = ' ') %>% 
      filter(! word1 %in% stopwords.df$word) %>% 
      filter(! word2 %in% stopwords.df$word) %>% 
      filter(! is.na(word1)) %>% 
      filter(! is.na(word2)) 
    
    skip.gram.count <- skip.gram.words  %>% 
      count(word1, word2, sort = TRUE) %>% 
      rename(weight = n)

    
    threshold <- 1
    
    network <-  skip.gram.count %>%
      filter(weight > threshold) %>%
      graph_from_data_frame(directed = FALSE)
    
    # Select biggest connected component.  
    V(network)$cluster <- clusters(graph = network)$membership
    
    cc.network <- induced_subgraph(
      graph = network,
      vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
    )
    
    # Store the degree.
    V(cc.network)$degree <- strength(graph = cc.network)
    # Compute the weight shares.
    E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)
    
    node.impo.df <- tibble(
      word = V(cc.network)$name,  
      degree = strength(graph = cc.network),
      closeness = closeness(graph = cc.network), 
      betweenness = betweenness(graph = cc.network)
    )
    
    node.impo.df
    
  } 
  
  
  node_info_df <- reactive({
    
    graph_distr(gcomb_senti3())
    
  })
  
  
  
  plt.deg <- reactive({
    
    node_info_df() %>% 
      ggplot(mapping = aes(x = degree)) +
      theme_light() +
      geom_histogram(fill = 'blue', alpha = 0.8, bins = 30)

  })
  

  
  plt.clo <- reactive({
    
    node_info_df() %>% 
      ggplot(mapping = aes(x = closeness)) +
      theme_light() +
      geom_histogram(fill = 'red', alpha = 0.8, bins = 30)
    
  })
  

  
  
  plt.bet <- reactive({
    
    node_info_df() %>% 
      ggplot(mapping = aes(x = betweenness)) +
      theme_light() +
      geom_histogram(fill = 'green4', alpha = 0.8, bins = 30) 
    
  })
  

  output$plot_deg <- renderPlot({plt.deg()})
  
  
  output$plot_clo <- renderPlot({plt.clo()})
  
  
  output$plot_bet <- renderPlot({plt.bet()})
  
  
 #Network Visualization
   
  graph_net <- function(gcomb_senti3) {
    
    skip.window <- 2
    
    skip.gram.words <- gcomb_senti3 %>% 
      unnest_tokens(
        input = item_title, 
        output = skipgram, 
        token = 'skip_ngrams', 
        n = skip.window
      ) %>% 
      filter(! is.na(skipgram))
    
    
    
    skip.gram.words$num_words <- skip.gram.words$skipgram %>% 
      map_int(.f = ~ ngram::wordcount(.x))
    
    skip.gram.words %<>% filter(num_words == 2) %>% select(- num_words)
    
    skip.gram.words %<>% 
      separate(col = skipgram, into = c('word1', 'word2'), sep = ' ') %>% 
      filter(! word1 %in% stopwords.df$word) %>% 
      filter(! word2 %in% stopwords.df$word) %>% 
      filter(! is.na(word1)) %>% 
      filter(! is.na(word2)) 
    
    skip.gram.count <- skip.gram.words  %>% 
      count(word1, word2, sort = TRUE) %>% 
      rename(weight = n)
    
    
    threshold <- 1
    
    network <-  skip.gram.count %>%
      filter(weight > threshold) %>%
      graph_from_data_frame(directed = FALSE)
    
    # Select biggest connected component.  
    V(network)$cluster <- clusters(graph = network)$membership
    
    cc.network <- induced_subgraph(
      graph = network,
      vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
    )
    
    # Store the degree.
    V(cc.network)$degree <- strength(graph = cc.network)
    # Compute the weight shares.
    E(cc.network)$width <- E(cc.network)$weight/max(E(cc.network)$weight)
    
    # Create networkD3 object.
    network.D3 <- igraph_to_networkD3(g = cc.network)
    # Define node size.
    network.D3$nodes %<>% mutate(Degree = 2*V(cc.network)$degree)
    # Degine color group (I will explore this feature later).
    network.D3$nodes %<>% mutate(Group = 1)
    # Define edges width. 
    network.D3$links$Width <- 10*E(cc.network)$width
    
    comm.det.obj <- cluster_louvain(
      graph = cc.network, 
      weights = E(cc.network)$weight
    )
    
    comm.det.obj
    
    V(cc.network)$membership <- membership(comm.det.obj)
    
    network.D3$nodes$Group <- V(cc.network)$membership
    
    fn3 <- forceNetwork(
      Links = network.D3$links, 
      Nodes = network.D3$nodes, 
      Source = 'source', 
      Target = 'target',
      NodeID = 'name',
      Group = 'Group', 
      opacity = 0.9,
      Value = 'Width',
      Nodesize = 'Degree', 
      # We input a JavaScript function.
      linkWidth = JS("function(d) { return Math.sqrt(d.value); }"), 
      fontSize = 12,
      zoom = TRUE, 
      opacityNoHover = 1
    )
    
    fn3
    

  }
  
  
  #Network D3 Output
  
  
  fnd3 <- reactive({
    
    graph_net(gcomb_senti3())
    
  })
  

  output$net <- renderForceNetwork({fnd3()})

}

shinyApp(ui, server)