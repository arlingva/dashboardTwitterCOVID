GetData <- function(vecUser, vn){
  # Crea un elemento vacío
  df <- c()
  for (i in 1:length(vecUser)){
    # Obtiene los datos de twitter en un elemento temporal
    temp <- userTimeline(vecUser[i], vn,
                         maxID=NULL, sinceID=NULL,
                         includeRts=FALSE, excludeReplies=FALSE) %>% 
      # Genera un data frame
      twListToDF()
    # se acumulan los datos de cada cuenta
    df <- rbind(df, temp)
  }
  return(df)
}