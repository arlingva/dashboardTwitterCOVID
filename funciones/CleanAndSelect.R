CleanAndSelect <- function(df, vecUser){
  # Se separa fecha y hora
  df <- df %>%
    separate(created, into = c("Fecha", "Hora"), sep = " ")

  # Cambia el formato de Fecha
  df$Fecha %<>% ymd()

  # Tratamiento de texto
  df$text <- df$text %>%
    # se convierte el texto a minúsculas
    tolower() %>%            
    # se eliminan páginas web y #
    str_replace_all("http\\S*", "") %>%
    str_replace_all("#S*", "") %>%       
    # se eliminan signos de puntuación y números
    str_replace_all("[[:punct:]]", " ") %>%   
    str_replace_all("[[:digit:]]", " ") %>%
    # Eliminación de espacios en blanco extra
    str_replace_all("[\\s]+", " ")

  # Se obtiene la fecha del primer tweet en el dataset para cada usuario
  fecha.inicial <- df %>%
    group_by(screenName) %>%
    summarise(Fecha.min = min(Fecha))
  
  # Se fija una fecha inicial en el máximo + día
  fecha.inicial$Fecha.min %<>% ymd()
  fecha.inicial <- max(fecha.inicial$Fecha.min) + 1    
  
  df <- df %>%
    filter(Fecha >= fecha.inicial) %>%
    filter(screenName %in% vecUser)  %>%
    select(id, screenName, Fecha, Hora, favoriteCount, retweetCount, text)
  
  return(df)
}