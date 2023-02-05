library(reactable)
library(tidyRSS)
library(tidyverse)
library(openxlsx)
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


  
  stopwords.df = tibble(
    word = c(stopwords(kind = 'en'))
  )
  
  #News Aggregation Function
  
  load_news = function() {
    
    gds_tidy = tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Fwww.thedailystar.net%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gpa_tidy = tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Fen.prothomalo.com%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gdt_tidy = tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Fwww.dhakatribune.com%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gna_tidy = tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Fwww.newagebd.net%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gun_tidy = tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Funb.com.bd%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gbn_tidy = tidyfeed("https://news.google.com/rss/search?q=site%3Ahttps%3A%2F%2Fbdnews24.com%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gfe_tidy = tidyfeed("https://news.google.com/rss/search?q=site%3Athefinancialexpress.com.bd%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    gjn_tidy = tidyfeed("https://news.google.com/rss/search?q=site%3Ajagonews24.com%20when%3A1d&hl=en-US&gl=US&ceid=US%3Aen")
    
    
    gcomb = bind_rows(gds_tidy, gpa_tidy, gdt_tidy, gna_tidy, gun_tidy,gbn_tidy,gfe_tidy,gjn_tidy)
    
    gcomb_rss = gcomb |>
      select(-item_category) |>
      #mutate(news_url = str_extract(feed_title, "(?<=//)[^\\s/:]+")) |>
      #mutate(news_url = str_replace(news_url, "//","")) |>
      #mutate(news_url = str_replace(news_url, "www.", "")) |>
      #mutate(news_url = str_extract(news_url, "[^/]+")) |>
      mutate(news_url = str_extract(item_title,'-[^-]+$')) |>
      mutate(news_url = sub("- ","", news_url)) |>
      mutate(item_title = sub('-[^-]+$', '', item_title)) |>
      mutate(item_title = sub('-[^-]+$', '', item_title))
    
    gcomb_rss = gcomb_rss |>
      select(item_title,item_link,item_pub_date,news_url)
    
    gcomb_rss
    
  }
  
  
  #News Aggregation
  
  newsx = load_news()
  
  
  dt_news = DT::datatable(newsx,
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
    
  
  
  #Sentiment Analysis Function
  
  news_senti = function(gcomb_rss) {
    
    gcb = gcomb_rss
    
    gcomb_senti1 = sentiment_by(gcomb_rss$item_title)
    
    gcomb_senti2 = bind_cols(gcb, gcomb_senti1)
    
    gcomb_senti3 = gcomb_senti2 |>
      mutate(polarity_level = ifelse(ave_sentiment > 0, "Positive", ifelse(ave_sentiment < 0, "Negative", ifelse(ave_sentiment == 0, "Neutral", "no"))))
    
    gcomb_senti3
    
  }
  
  gcomb_senti3 = news_senti(newsx)
  
  #Sentiment Analysis boxplot
  
  
  gcomb_box = gcomb_senti3 |>
      ggplot() + 
      geom_boxplot(aes(y=news_url, x = ave_sentiment))
  
  #Sentiment Analysis barplot
  
  gcomb_col = gcomb_senti3 |>
      count(news_url,polarity_level) |>
      ggplot() + geom_col(aes(y = news_url, x = n, fill = polarity_level)) + theme_minimal()
  

  gcomb_col_pl = ggplotly(gcomb_col)
  
  #Tidy Data
  
  gtd = function(gcomb_senti3){
    
    gcomb_tidy = gcomb_senti3 |>
      mutate(item_title_v2 = item_title) |>
      unnest_tokens(word,item_title) |>
      anti_join(stop_words)
    
    gcomb_tidy
    
  }
  
  
  
  gcomb_tidy = gtd(gcomb_senti3)
  
  #Tidy Data Summary Table
  
  td_summary = function(gcomb_tidy) {
    
    word_summary = gcomb_tidy |>
      mutate(news_url = ifelse(is.na(news_url),"NONE", news_url)) |>
      group_by(news_url, item_title_v2) |>
      mutate(word_count = n_distinct(word)) |>
      select(item_title_v2, Newspaper = news_url, Polarity = polarity_level, word_count) |>
      distinct() |> #To obtain one record per song
      ungroup()
    
    word_summary
    
  }
  
  
  word_summary = td_summary(gcomb_tidy)
  
  
  td_summary = DT::datatable(word_summary,
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
  
  #NRC Emotion Plot
  
  nrc = function(gcomb_tidy) {
    
    gcomb_nrc = gcomb_tidy |>
      inner_join(get_sentiments("nrc"))
    
  }
  
  
  gcomb_nrc = nrc(gcomb_tidy)
  
  
  theme_lyrics = function(aticks = element_blank(),
                           pgminor = element_blank(),
                           lt = element_blank(),
                           lp = "none")
  {
    theme(plot.title = element_text(hjust = 0.5),
          axis.ticks = aticks,
          panel.grid.minor = pgminor,
          legend.title = lt,
          legend.position = lp)
  }
  
  
  emo_plot = function(gcomb_nrc) {
    
    plot_words = gcomb_nrc |>
      group_by(sentiment) |>
      count(word, sort = TRUE) |>
      arrange(desc(n)) |>
      slice(seq_len(8)) |> 
      ungroup()
    
    word_plots = plot_words |>
      
      ggplot(aes(word, 1, label = word, fill = sentiment )) +
      
      geom_point(color = "transparent") +
      
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
  
  emotion_plot = emo_plot(gcomb_nrc)
  

  nrc_pl = function(gcomb_nrc) {
    
    nrc_plot = gcomb_nrc |>
      group_by(sentiment) |>
      summarise(word_count = n()) |>
      ungroup() |>
      mutate(sentiment = reorder(sentiment, word_count)) |>
      ggplot(aes(sentiment, word_count, fill = -word_count)) +
      geom_col() +
      guides(fill = FALSE) + 
      theme_lyrics() +
      labs(x = NULL, y = "Word Count") +
      ggtitle("News NRC Sentiment") +
      coord_flip()
    
    nrc_plot
    
    
  }
  
  plot_nrc = nrc_pl(gcomb_nrc)
  
  plot_nrc_pl = ggplotly(plot_nrc)

  
  #Network Analysis
  
  # skipgram distribution
  
  graph_distr = function(gcomb_senti3) {
    
    skip.window = 2
    
    skip.gram.words = gcomb_senti3 |> 
      unnest_tokens(
        input = item_title, 
        output = skipgram, 
        token = 'skip_ngrams', 
        n = skip.window
      ) |> 
      filter(! is.na(skipgram))
    
    
    
    skip.gram.words$num_words = skip.gram.words$skipgram |> 
      map_int(.f = ~ ngram::wordcount(.x))
    
    skip.gram.words %<>% filter(num_words == 2) |> select(- num_words)
    
    skip.gram.words %<>% 
      separate(col = skipgram, into = c('word1', 'word2'), sep = ' ') |> 
      filter(! word1 %in% stopwords.df$word) |> 
      filter(! word2 %in% stopwords.df$word) |> 
      filter(! is.na(word1)) |> 
      filter(! is.na(word2)) 
    
    skip.gram.count = skip.gram.words  |> 
      count(word1, word2, sort = TRUE) |> 
      rename(weight = n)
    
    
    threshold = 1
    
    network =  skip.gram.count |>
      filter(weight > threshold) |>
      graph_from_data_frame(directed = FALSE)
    
    # Select biggest connected component.  
    V(network)$cluster = clusters(graph = network)$membership
    
    cc.network = induced_subgraph(
      graph = network,
      vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
    )
    
    # Store the degree.
    V(cc.network)$degree = strength(graph = cc.network)
    # Compute the weight shares.
    E(cc.network)$width = E(cc.network)$weight/max(E(cc.network)$weight)
    
    node.impo.df = tibble(
      word = V(cc.network)$name,  
      degree = strength(graph = cc.network),
      closeness = closeness(graph = cc.network), 
      betweenness = betweenness(graph = cc.network)
    )
    
    node.impo.df
    
  } 
  
  
  node_info_df = graph_distr(gcomb_senti3)
  
  
  plt.deg = node_info_df |> 
      ggplot(mapping = aes(x = degree)) +
      theme_light() +
      geom_histogram(fill = 'blue', alpha = 0.8, bins = 30)
  
  
  plt.clo = node_info_df |> 
      ggplot(mapping = aes(x = closeness)) +
      theme_light() +
      geom_histogram(fill = 'red', alpha = 0.8, bins = 30)
  
  
  
  
  plt.bet = node_info_df |> 
      ggplot(mapping = aes(x = betweenness)) +
      theme_light() +
      geom_histogram(fill = 'green4', alpha = 0.8, bins = 30)
  

  
  #Network Visualization
  
  graph_net = function(gcomb_senti3) {
    
    skip.window = 2
    
    skip.gram.words = gcomb_senti3 |> 
      unnest_tokens(
        input = item_title, 
        output = skipgram, 
        token = 'skip_ngrams', 
        n = skip.window
      ) |> 
      filter(! is.na(skipgram))
    
    
    
    skip.gram.words$num_words = skip.gram.words$skipgram |> 
      map_int(.f = ~ ngram::wordcount(.x))
    
    skip.gram.words %<>% filter(num_words == 2) |> select(- num_words)
    
    skip.gram.words %<>% 
      separate(col = skipgram, into = c('word1', 'word2'), sep = ' ') |> 
      filter(! word1 %in% stopwords.df$word) |> 
      filter(! word2 %in% stopwords.df$word) |> 
      filter(! is.na(word1)) |> 
      filter(! is.na(word2)) 
    
    skip.gram.count = skip.gram.words  |> 
      count(word1, word2, sort = TRUE) |> 
      rename(weight = n)
    
    
    threshold = 1
    
    network =  skip.gram.count |>
      filter(weight > threshold) |>
      graph_from_data_frame(directed = FALSE)
    
    # Select biggest connected component.  
    V(network)$cluster = clusters(graph = network)$membership
    
    cc.network = induced_subgraph(
      graph = network,
      vids = which(V(network)$cluster == which.max(clusters(graph = network)$csize))
    )
    
    # Store the degree.
    V(cc.network)$degree = strength(graph = cc.network)
    # Compute the weight shares.
    E(cc.network)$width = E(cc.network)$weight/max(E(cc.network)$weight)
    
    # Create networkD3 object.
    network.D3 = igraph_to_networkD3(g = cc.network)
    # Define node size.
    network.D3$nodes %<>% mutate(Degree = 2*V(cc.network)$degree)
    # Degine color group (I will explore this feature later).
    network.D3$nodes %<>% mutate(Group = 1)
    # Define edges width. 
    network.D3$links$Width = 10*E(cc.network)$width
    
    comm.det.obj = cluster_louvain(
      graph = cc.network, 
      weights = E(cc.network)$weight
    )
    
    comm.det.obj
    
    V(cc.network)$membership = membership(comm.det.obj)
    
    network.D3$nodes$Group = V(cc.network)$membership
    
    fn3 = forceNetwork(
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
  
  
  fnd3 = graph_net(gcomb_senti3)
  
  
  
  
  
  graph_net_bi = function(gcomb_senti3) 
    
  {
    
    bi.gram.words = gcomb_senti3 |> 
      unnest_tokens(
        input = item_title, 
        output = bigram, 
        token = 'ngrams', 
        n = 2
      ) |> 
      filter(! is.na(bigram))
    
    bi.gram.words %<>% 
      separate(col = bigram, into = c('word1', 'word2'), sep = ' ') |> 
      filter(! word1 %in% stopwords.df$word) |> 
      filter(! word2 %in% stopwords.df$word) |> 
      filter(! is.na(word1)) |> 
      filter(! is.na(word2)) 
    
    
    bi.gram.count = bi.gram.words |> 
      count(word1, word2, sort = TRUE) |> 
      # We rename the weight column so that the 
      # associated network gets the weights (see below).
      rename(weight = n)
    
    
    threshold = 0
    
    network =  bi.gram.count |>
      filter(weight > threshold) |>
      graph_from_data_frame(directed = FALSE)
    
    V(network)$degree = strength(graph = network)
    # Compute the weight shares.
    E(network)$width = E(network)$weight/max(E(network)$weight)
    
    network.D3 = igraph_to_networkD3(g = network)
    # Define node size.
    network.D3$nodes %<>% mutate(Degree = (10)*V(network)$degree)
    # Define edges width. 
    network.D3$links$Width = 10*E(network)$width
    
    
    comm.det.obj = cluster_louvain(
      graph = network, 
      weights = E(network)$weight
    )
    
    V(network)$membership = membership(comm.det.obj)
    
    network.D3$nodes$Group = V(network)$membership
    
    fn_bi = forceNetwork(
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
    
    fn_bi

  }
  
  fnd_bi = graph_net_bi(gcomb_senti3)
  
      