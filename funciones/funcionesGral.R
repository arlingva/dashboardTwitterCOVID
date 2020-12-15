tema_graf <-
  theme_minimal() +
  theme(text = element_text(family = "serif"),
        panel.grid.minor = element_blank(),
        strip.background = element_rect(fill = "#EBEBEB", colour = NA),
        legend.position = "none",
        legend.box.background = element_rect(fill = "#EBEBEB", colour = NA))

limpiezaDatos <- function(texto){
  #Replacing "/", "@" and "|" with space
  toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
  texto <- tm_map(texto, toSpace, "/")
  texto <- tm_map(texto, toSpace, "@")
  texto <- tm_map(texto, toSpace, "\\|")
  # Convert the text to lower case
  texto <- tm_map(texto, content_transformer(tolower))
  # Remove numbers
  texto <- tm_map(texto, removeNumbers)
  # Remove english common stopwords
  texto <- tm_map(texto, removeWords, stopwords("english"))
  # Remove your own stop word
  # specify your custom stopwords as a character vector
  texto <- tm_map(texto, removeWords, c("s", "company", "team")) 
  # Remove punctuations
  texto <- tm_map(texto, removePunctuation)
  # Eliminate extra white spaces
  texto <- tm_map(texto, stripWhitespace)
  # Text stemming - which reduces words to their root form
  texto <- tm_map(texto, stemDocument)
  texto <- str_replace_all(texto, "http\\S*", "")
}


limpieza <- function(texto){
  # se convierte el texto a minúsculas
  texto <- tolower(texto)
  #texto <- str_replace_all(texto, "s$ ", "$")
  # se eliminan páginas web
  texto <- str_replace_all(texto, "http\\S*", "")
  # se eliminan signos de puntuación y números
  texto <- str_replace_all(texto, "[[:punct:]]", " ")
  texto <- str_replace_all(texto, "[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  texto <- str_replace_all(texto, "[\\s]+", " ")
  # Tokenización por palabras individuales
  #texto <- str_split(texto, " ")[[1]]
  # Eliminación de tokens con una longitud < 2
  texto <- keep(.x = texto, .p = function(x){str_length(x) > 1})
  return(texto)
}

codificacion <-function(df){
  df <- df %>%
    mutate(SEXO = recode(SEXO,
                         `1` = "Mujer", 
                         `2` = "Hombre",
                         `99` = "No especificó"),
           RESULTADO_ANTIGENO = recode(RESULTADO_ANTIGENO,
                                       `1` = "Positivo a SARS-COV-2",
                                       `2` = "Negativo a SARS-COV-2",
                                       `97` = "NA (caso sin muestra)")
           )
  return(df)
}


