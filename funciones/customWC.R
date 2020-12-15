customWC <- function(user, df){
  df.wc <- df.palabras %>% 
    filter(!(Palabra %in% stopwords(kind = "es"))) %>%
    group_by(screenName, Palabra) %>% 
    count(Palabra) %>%
    group_by(screenName) %>% 
    mutate(frecuencia = n / n()) %>%
    arrange(screenName, desc(frecuencia)) %>% 
    filter(screenName == user)
  
  wordcloud(words = df.wc$Palabra, freq = df.wc$frecuencia, 
            max.words = 100, colors = brewer.pal(8, "Dark2"),
            random.order = FALSE, rot.per = 0.35, scale = c(3, 0.3))
}
