# aseguramos que paquetes estén nstalados
if(!require("dplyr")) install.packages("dplyr")
if(!require("stringr")) install.packages("stringr")
if(!require("curl")) install.packages("curl")
if(!require("purrr")) install.packages("purrr")
if(!require("data.table")) install.packages("data.table")

## evitamos notación científica
options(scipen=999)

### cargamos paquetes
library(data.table)
library(stringr)
library(dplyr)
library(curl)
library(purrr)

## Solucionario Tarea final


# Ejercicio 1:descargar archivos

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)


# Lo primero que haremos será encontrar los nombres de los archivos dentro de las url. 
# Utilizando expresiones regulares, crea un vector llamado file_names que almacene la porción de las url 
# que contiene el nombre de los archivos que luego descargaremos (ej: esi-2021—personas.csv). 
# Para esto, crea una función llamada extract_name. extract_name debe recibir una url y devolver el nombre del archivo.
# ¡No olvides utilizar purrr en tu solución para el vector de nombres!

file_names <- str_extract_all(urls,pattern = "esi-\\d+---personas.csv")

# Crea una función llamada download_esi_data para descargar un archivo. La función debe recibir 3 parámetros: url, file_name y directory.

## descarga datos 
download_esi_data <- function(url, file_name, directory){
  
  file_directory <- paste0(directory, "/", file_name)
  curl::curl_download(url,  file_directory)
}

# ++++ Usando purrr, las url y el vector de nombres, descarga todos los archivos en una carpeta llamada data en tu directorio de trabajo. 
# ++++ No está permitido el uso de rutas absolutas

## Descargamos 
walk2(urls, file_names, ~download_esi_data(url = .x, file_name = .y, directory = "tarea_final/data"))

# Ejercicio 2: leer archivos

# Ahora cargaremos los archivos en la sesión. Recuerda que no necesariamente todos los archivos tienen el mismo separador.

# Crea una función llamada read_esi_data que lea un archivo. La función recibe como argumento la ruta del archivo (ej: data/esi-2018—personas.csv). 
# read_esi_data debe ser capaz de reconocer el tipo de separador y leer el archivo correctamente en todos los casos. Para lograr esto existen varios caminos.


read_esi_data <- function(ruta){
  map(list.files(ruta,full.names=T),fread)
}

datos <- read_esi_data("tarea_final/data/")

names(datos) <- file_names %>% stringr::str_remove_all("---personas.csv") %>% stringr::str_replace_all("-","_")

# Ejercicio 3: obtener datos

# Tu jefatura está interesada en conocer algunas características sobre las variables de diseño y sobre la variable principal de ingresos (ing_t_p). 
# Para ello, te solicita lo siguiente:

   
# Tabla que contenga 3 columnas: version, n_personas (idrph) y n_hogares (id_identificacion). En la columna version debes usar la siguiente estructura: esi_{año}. Ejemplo: esi_2017

map2(datos,names(datos),~.x %>% 
       summarise(n_personas = n_distinct(idrph),
                 n_hogares = n_distinct(id_identificacion)) %>% 
       mutate(version=.y) %>%
       relocate(version)
       ) %>%  
  bind_rows() %>% 
  arrange(version)


# Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 del factor de expansión (fact_cal_esi) para cada versión. Debes considerar una fila por hogar (id_identificacion) e incluir la columna version ¿Se observan algunos pesos de muestreo atípicos?

map2(datos,names(datos),~.x %>% 
       mutate(fact_cal_esi = as.numeric(str_replace_all(fact_cal_esi,",","."))) %>% #select(fact_cal_esi) %>% 
       group_by(id_identificacion) %>% 
       summarise(min = min(fact_cal_esi,na.rm = T),
                 max = max(fact_cal_esi,na.rm = T),
                 mean = mean(fact_cal_esi,na.rm = T),
                 median = median(fact_cal_esi,na.rm = T),
                 p10 = quantile(fact_cal_esi,.1,na.rm = T),
                 p90 = quantile(fact_cal_esi,na.rm = T,.9)) %>% 
       mutate_all(as.numeric) %>% 
       mutate(version=.y) %>%
       relocate(version)) %>%  
  bind_rows()

# Tabla que contenga el número de estratos (estrato) con una sola unidad primaria de muestro (conglomerado). Debes incluir la columna version.

map2(datos,names(datos),~.x %>% 
  group_by(conglomerado) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(estrato) %>% 
  dplyr::filter(n==1) %>% 
  count %>% 
  mutate(version=.y) %>%
  relocate(version)) %>%  
  bind_rows()
  

# Tabla que contenga mínimo, máximo, media, mediana, p10 y p90 de los ingresos del trabajo principal a nivel de personas (ing_t_p) para cada versión.

map2(datos,names(datos),~.x %>% 
       mutate(ing_t_p = as.numeric(str_replace_all(ing_t_p,",","."))) %>% #select(ing_t_p) %>% 
       group_by(idrph) %>% 
       summarise(min = min(ing_t_p,na.rm = T),
                 max = max(ing_t_p,na.rm = T),
                 mean = mean(ing_t_p,na.rm = T),
                 median = median(ing_t_p,na.rm = T),
                 p10 = quantile(ing_t_p,.1,na.rm = T),
                 p90 = quantile(ing_t_p,na.rm = T,.9)) %>% 
       mutate_all(as.numeric) %>% 
       mutate(version=.y) %>%
       relocate(version)) %>%  
  bind_rows()

# Ejercicio 4: mejorando el código
# Tu jefatura está muy satisfecha con los resultados obtenidos, pero está preocupada por el tiempo de ejecución y te pregunta si es posible crear un código más eficiente. Para ello, te solicita comparar el tiempo de ejecución de algunas estrategias. Utiliza el paquete microbenchmark, cuyo uso básico se describe aquí.
 
library(microbenchmark)

# Nota: No es necesario que utilices más de 5 iteraciones para calcular el tiempo de ejecución.
 
# Calcula el promedio de ingresos en las tablas de la ESI (ing_t_p) mediante las siguientes estrategias:
   
# Lista de tablas: calcular promedio con herramientas de purrr (como en el ejercicio anterior)

# Tablas apiladas: calcular promedio con group_by() %>% summarise() (apila una tabla sobre otra en un dataframe)
 
# Lista de tablas: calcular promedio con herramientas de purrr, utilizando una función creada por ti, que utilice data.table.
 
# Tablas apiladas: calcular promedio con data.table.

# ¿Existen diferencias importantes entre las distintas estrategias? ¿Hay alguna más eficiente que otra? ¿Usar group_by versus map hace alguna diferencia?

## corregimos variable con comas
datos[[4]]$ing_t_p = str_replace_all(datos[[4]]$ing_t_p,",",".") %>% as.numeric

mean_dt <- function(df){
  df[,mean(ing_t_p,na.rm = T)]
}


microbenchmark(
  purr_test_ing = map(datos,~.x %>% pull(ing_t_p) %>% mean),
  dplyr_test_ing = bind_rows(datos[[1]] %>% mutate(version = names(datos)[1]) %>% 
                               select(version,ing_t_p),
                             datos[[2]] %>%
                               mutate(version = names(datos)[2]) %>% 
                               select(version,ing_t_p),
                             datos[[3]] %>%
                               mutate(version = names(datos)[3]) %>% 
                               select(version,ing_t_p),
                             datos[[4]] %>%
                               mutate(version = names(datos)[4]) %>% 
                               select(version,ing_t_p),
                             datos[[5]] %>%
                               mutate(version = names(datos)[5]) %>% 
                               select(version,ing_t_p),
                             datos[[6]] %>%
                               mutate(version = names(datos)[6]) %>% 
                               select(version,ing_t_p)) %>% 
    group_by(version) %>% 
    summarise(mean = mean(ing_t_p,na.rm=T)),
  purrr_data.table_ing = map(datos,mean_dt) %>% bind_cols(),
  data.table_ing = rbind(datos[[1]][,mean(ing_t_p,na.rm = T)],
        datos[[2]][,mean(ing_t_p,na.rm = T)],
        datos[[3]][,mean(ing_t_p,na.rm = T)],
        datos[[4]][,mean(ing_t_p,na.rm = T)],
        datos[[5]][,mean(ing_t_p,na.rm = T)],
        datos[[6]][,mean(ing_t_p,na.rm = T)])
  , times = 5)





rbind(datos[[1]][,mean(ing_t_p,na.rm = T)],
      datos[[2]][,mean(ing_t_p,na.rm = T)],
      datos[[3]][,mean(ing_t_p,na.rm = T)],
      datos[[4]][,mean(ing_t_p,na.rm = T)],
      datos[[5]][,mean(ing_t_p,na.rm = T)],
      datos[[6]][,mean(ing_t_p,na.rm = T)])







