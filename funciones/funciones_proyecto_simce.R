## Funciones proyecto ##
#nota: este script agrupa las funciones del proyecto modelo-simce y que se utilizan durante todo el proceso 



# Generales -----

## generar ruta ----
### Genera ruta según usuario que usa el proyecto 

generar_ruta <- function(){
  
  usuario <- Sys.info()[["user"]]
  rutas <- config::get(config = usuario, file = "config.yml")
  
  ruta_data_in <- rutas$ruta_data_in
  ruta_data_intermedia <- rutas$ruta_data_intermedia
  
  return(
    list(
      ruta_data_in = rutas$ruta_data_in
      ,ruta_data_intermedia = rutas$ruta_data_intermedia
      ,ruta_outputs = rutas$ruta_outputs
      )
  )
  
}

## Generar data pre reporte irt ----
### Genera la unificación de datos previo a los reportes irt

generar_data_pre_irt <- function(data,n_ensayos,asignatura,nivel){
  
  require(purrr)
  require(readr)
  require(tidyverse)
  require(arrow)
  require(dplyr)
  # Generar selección por nivel y separar po número de ensayo 
  data_por_ensayo_materia<-map(n_ensayos  
                               ,~data[str_detect(names(data)
                                                           ,paste0(asignatura
                                                                   ,"_"
                                                                   ,nivel
                                                                   ,"_ensayo",.x))]
  )
  #unificar por n ensayo para ingresar a modelo irt  
  data_unificada <- map(n_ensayos
                        ,~bind_rows(data_por_ensayo_materia[[.x]])
                        
  )
  
  return(data_unificada)  
  
}

## Generar modelo IRT ----
generar_modelo_irt <- function(data_input,asignatura,ensayos){
  require(purrr)
  require(readr)
  require(tidyverse)
  require(arrow)
  require(dplyr)
  # Ajustar nombre de variables 
  data_mirt <- data_input[[asignatura]][[ensayos]] %>%
    rename("nombre_apellido" = "nombre_y_apellido"
           ,"cod_item"="item"
           ,"item" = "item_no"
           ,"alt_correcta" = "alternativa_correcta"
           ,"alt_incorrecta" = "alternativa_incorrecta") %>% 
    mutate(rev_item = alt_correcta)
  
  pdt_irt <- data_mirt %>%
    mutate(rev_item = alt_correcta ) %>% 
    dplyr::select(nombre_apellido, item, rev_item,curso ) %>%
    arrange(item) %>%  # Ordenar por ítem de manera ascendente
    group_by(nombre_apellido) %>%
    pivot_wider(names_from = "item",
                values_from = "rev_item",
                names_prefix = "item_") %>%
    ungroup() %>% 
    dplyr::select(-c(nombre_apellido,curso)) 
  

  
  
    
  
  # Convertir cada columna de `pdt_irt` en vector numérico

  pdt_irt <- as.data.frame(lapply(pdt_irt, function(col) as.numeric(unlist(col))))
  
  # Generar modelo irt
  pdt_irt3pl <- tpm(pdt_irt, type = "latent.trait",IRT.param = TRUE)
  
  print("se crea modelo tres parámetros")
  
  # Generar data con los coeficientes
  data_resumen<-coef(pdt_irt3pl, prob = TRUE) %>% 
    as.data.frame() %>%
    clean_names() %>% 
    summarise(
      mean_gussng = round(mean(gussng),2)
      ,min_gussng = round(min(gussng),2)
      ,max_gussng = round(max(gussng),2)
      ,mean_dffclt= round(mean(dffclt),2)
      ,min_dffclt= round(min(dffclt),2)
      ,max_dffclt= round(max(dffclt),2)
      ,.groups = "drop"
    ) %>% 
    mutate(
      n_ensayo = ensayos
    )
  
  print("fin ejecución tabulado resumen")
  
  
  
  #salida función 
  return(data_resumen)
  
  
}

# Conversión porcentaje de logro -  puntaje simce ----

convertir_logro_simce_modelo1 <- function(data, tabla_conversion){
  
  require(tidyverse)
  require(dplyr)
  require(readxl)
  
  data<-data |>
    mutate(
      porcentaje_logro_sin_decimal = round(porcentaje_logro)
    ) |> 
    left_join(
      tabla_conversion |> 
        rename(
          "puntaje_simce_leng"="puntaje_lect4b"
          ,"puntaje_simce_mat"="puntaje_mate4b"
        )
      ,by = c("porcentaje_logro_sin_decimal"="porc_lect4b")
    ) |> 
    mutate(
     puntaje_simce_modelo1 = case_when(
       area == "lenguaje" ~puntaje_simce_leng
       ,area == "matematica" ~puntaje_simce_mat
     )
   )
  return(data)
}







