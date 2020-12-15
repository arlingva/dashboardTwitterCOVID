codigosCOVID <-function(df){
  df <- df %>% 
  mutate(cve_mpo = str_c(entidad_res, "-", municipio_res))
         
         # Unir catálogo de municipios y genera variable "municipio" 
         df <- df %>%
           left_join(cve_mpo %>% select(municipio, cve_mpo),
                     by = "cve_mpo") %>%
           mutate(municipio = str_to_title(municipio),
                  municipio = str_replace(municipio, " De ", " de "),
                  municipio = str_replace(municipio, " Del ", " del "),
                  municipio = str_replace(municipio, " Los ", " los "),
                  municipio = str_replace(municipio, " La ", " la ")) 
  return(df)
}
