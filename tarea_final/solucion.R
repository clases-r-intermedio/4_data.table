
if(!require("dplyr")) install.packages("dplyr")
if(!require("stringr")) install.packages("stringr")

library(stringr)

## Solucionario Tarea final

urls <- c("https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2021/esi-2021---personas.csv?sfvrsn=d03ae552_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2020/esi-2020---personas.csv?sfvrsn=fb1f7e0c_4&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2019/esi-2019---personas.csv?sfvrsn=9eb52870_8&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2018/esi-2018---personas.csv?sfvrsn=a5de2b27_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2017/esi-2017---personas.csv?sfvrsn=d556c5a1_6&download=true",
          "https://www.ine.cl/docs/default-source/encuesta-suplementaria-de-ingresos/bbdd/csv_esi/2016/esi-2016---personas.csv?sfvrsn=81beb5a_6&download=true"
)


file_names <- str_extract_all(urls,pattern = "esi-\\d+---personas.csv")




