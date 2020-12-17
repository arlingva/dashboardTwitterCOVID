codigosCOVID <-function(df){
  cve_mpo <-read_excel("data/201128 Catalogos.xlsx", 
                       sheet = "Catálogo MUNICIPIOS") %>% 
    clean_names() %>% 
    mutate(cve_mpo = str_c(as.integer(clave_entidad),
                           "-",
                           as.integer(clave_municipio)
                           )
    )

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
                  municipio = str_replace(municipio, " La ", " la "),
                  municipio = str_replace(municipio, " Y ", " y ")) 
  return(df)
}
