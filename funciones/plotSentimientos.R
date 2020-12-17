plotSentimientos <- function(sentimiento) {
  df.palabras %>%
    filter(Tipo ==  sentimiento) %>%
    group_by(screenName) %>%
    count(Palabra, sort = T) %>%
    arrange(-n, screenName) %>%
    top_n(n = 10, wt = n) %>%
    ggplot() +
    aes(Palabra, n, fill = screenName) +
    geom_col()+ 
    facet_wrap("screenName", scales = "free") +
    scale_y_continuous(expand = c(0, 0)) +
    coord_flip() +
    labs(title = NULL) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          strip.background = element_rect(fill = "gray", colour = NA),
          legend.position = "none"
    )
}