library(data.table)
library(dplyr)

# install.packages("data.table")

# ### cargamos datos
# censo <- data.table::fread("Untitled/csv-personas-censo-2017/Microdato_Censo2017-Personas.csv")
# censo <- janitor::clean_names(censo)
# dt_censo <- as.data.table(censo)

# podemos comparar el tiempo de ejecución entre read.csv y fread()
# tictoc::tic()
df_censo_viviendas <- readr::read_csv2("data/viviendas/Microdato_Censo2017-Viviendas.csv")
# tictoc::toc()
# 66.472 sec elapsed

tictoc::tic()
censo_viviendas <- data.table::fread("data/viviendas/Microdato_Censo2017-Viviendas.csv")
tictoc::toc()
# 2.968 sec elapsed

censo_viviendas

censo_viviendas <- janitor::clean_names(censo_viviendas)
names(censo_viviendas)

censo_viviendas[1:6,1:10]

censo_viviendas[1:6]


# R base
censo_viviendas[censo_viviendas$region == 13,]

# data.table
censo_viviendas[region == 13]

dt_viviendas <- censo_viviendas

dt_viviendas[region == 13 & area == 1]

### grabamos datos

# tictoc::tic()
#readr::write_csv(censo_viviendas,"csv-viviendas-censo-2017/ejemplo_viviendas.csv")
# tictoc::toc()
# 99.184 sec elapsed
# tictoc::tic()
#data.table::fwrite(censo_viviendas,"csv-viviendas-censo-2017/ejemplo_viviendas.csv")
# tictoc::toc()
# 1.17 sec elapsed

### convertimos en Data.table
tictoc::tic()
dt_viviendas <- data.table(censo_viviendas)   
tictoc::toc()

# tictoc::tic()
# dt_viviendas <- as.data.table(censo_viviendas)   
# tictoc::toc()

data.table::setDT()

### sintaxis básica, filtrar columnas por indexación

dt_viviendas[1:6,]

# o

dt_viviendas[1:6]

### filtros con data.table

tictoc::tic()
censo_viviendas %>% filter(region == 13 & area == 1)
tictoc::toc()

tictoc::tic()
dt_viviendas[region == 13 & area == 1]
tictoc::toc()


dt_viviendas[order(region,decreasing = T),]


### seleccionamos variables con data.table

dt_viviendas[,"region"]

# o

dt_viviendas[,c("region","area")]

dt_viviendas[]

# Parte J: ####
## aplicamos funciones 
###

### Funciones específicas  para resumir datos.
### calculamos la cantidad total de hogares
dt_viviendas[,sum(cant_hog)]

# calculamos el promedio de hogares por viviendas
dt_viviendas[,mean(cant_hog)]

### hacemos una tabla
dt_viviendas[,table(area)]

#### uniendo funciones de resumen 
dt_viviendas[,.(mean(cant_hog),sum(cant_hog))]

#### asignando nuevos nombres a funciones de resumen 
dt_viviendas[cant_hog != 98,.(promedio_hogares = mean(cant_hog),suma_hogares = sum(cant_hog), min_hog = min(cant_hog),max_hog = max(cant_hog))]


# Mediante el operador `:=` también podemos crear nuevas columnas:

dt_viviendas[,hacinamiento := p04/cant_per]

censo_viviendas <- censo_viviendas %>% 
  mutate(hacinamietno = p04/cant_per)

dt_viviendas[,hacinamiento := NULL]

dt_viviendas[,.(hacinamiento = p04/cant_per)]

dt_viviendas[,c("p04","cant_per","hacinamiento")]

tictoc::tic()
dt_viviendas[,hacinamiento := p04/cant_per]
tictoc::toc()

tictoc::tic()
censo_viviendas <- censo_viviendas %>% mutate(hacinamiento = p04/cant_per)
tictoc::toc()

#### fifelse(test, yes, no, na=NA)

dt_viviendas[,hacinamiento := fifelse(p04 != 98,p04/cant_per,NA_real_,na= NA)]
dt_viviendas[,c("p04","cant_per","hacinamiento")]

dt_viviendas[,mean(hacinamiento,na.rm = T)]

#### también posee una alternativa para case_when()
tictoc::tic()
dt_censo[, residencia_habitual := fcase(p10 == 1, "En esta vivienda",
                                     p10 == 2, "En otra vivienda",
                                     p10 == 3, "En otra comuna",
                                     p10 == 4, "En otro país",
                                     default = "missing"
                                     )][]

tictoc::toc()

# 0.36 sec elapsed

dt_censo[, residencia_habitual :=NULL]

tictoc::tic()
censo %>% 
  mutate(residencia_habitual = case_when(p10 == 1 ~"En esta vivienda",
                                     p10 == 2 ~"En otra vivienda",
                                     p10 == 3 ~"En otra comuna",
                                     p10 == 4 ~"En otro país"
  ))
tictoc::toc()

# 2.33 sec elapsed


dt_censo %>% 
  mutate(residencia_habitual = fcase(p10 == 1, "En esta vivienda",
                                     p10 == 2, "En otra vivienda",
                                     p10 == 3, "En otra comuna",
                                     p10 == 4, "En otro país"
  ))


#### asignando nuevos nombres a funciones de resumen 
dt_viviendas[cant_hog != 98,.(promedio_hogares = mean(cant_hog),suma_hogares = sum(cant_hog), min_hog = min(cant_hog),max_hog = max(cant_hog))]

dt_viviendas[,cant_hog2 := fifelse(cant_hog != 98,cant_hog,NA_real_)]

dt_viviendas[,.(promedio_hogares = mean(cant_hog),suma_hogares = sum(cant_hog), min_hog = min(cant_hog),max_hog = max(cant_hog2, na.rm = T))]

dt_viviendas[cant_hog == 36, c("region", "comuna")]


### Parte BY ####

tictoc::tic()
dt_viviendas[,mean(cant_per),by = comuna]
tictoc::toc()

tictoc::tic()
censo_viviendas %>% group_by(comuna) %>% summarise(prom_viv = mean(nviv))
tictoc::toc()

dt_viviendas[,.N,by = area]

dt_viviendas[,.N,by = .(area, comuna)]

tictoc::tic()
dt_viviendas[,.N,by = .(area, comuna)]
tictoc::toc()

tictoc::tic()
censo_viviendas %>% group_by(area,comuna) %>% count()
tictoc::toc()


### cargamos datos de personas 
censo <- data.table::fread("data/personas/Microdato_Censo2017-Personas.csv")
censo <- janitor::clean_names(censo)
dt_censo <- as.data.table(censo)

## .SD 
# como funciona por detras
dt_iris[,print(.SD),by = Species]

dt_iris <- data.table(iris)

# extraemos la 1 primera filas de cada especie
dt_iris[,.SD[1],by=Species]

# extraemos la ultima fila de cada especie
dt_iris[,.SD[.N],by=Species]

# si queremos extraer la fila con el petalo mas largo por especie
dt_iris[,.SD[which.min(Petal.Length)],by=Species]

## similar en dplyr a:
iris %>%
  group_by(Species) %>%
  arrange(Petal.Length) %>%
  slice(1)

# utilizando .SDcols
# extraemos las 3 primeras filas de cada especie, no es algo tan novedoso
dt_iris[,.SD[1:3],.SDcols=1:3]

# utilizando .SDcols
# extraemos las 3 primeras filas de las variables que tengan la palabra Sepal, es un poco mas novedoso
dt_iris[,.SD[1:3],.SDcols=patterns("Sepal")]

# extraemos la primera fila de cada especie, es un poco mas novedoso
dt_iris[,lapply(.SD,mean),.SDcols=patterns("Sepal"), by = Species]

un<- Sys.time()
dt_iris[,lapply(.SD,mean),by = Species]
do<- Sys.time()
do -un

un<- Sys.time()
dt_iris[,purrr::map(.SD,mean),by = Species]
do<- Sys.time()
do-un

### parametro keyby

dt_viviendas[,.N,by=region]


dt_viviendas[,.N,keyby=region]

## utilizamos dos puntos 

dt_viviendas[,.N,by=p01:p03c]

dt_viviendas[,.N,keyby=p01:p03c]


tictoc::tic()
dt_censo[,.N,by = .(provincia,comuna,dc,area,nviv)]
tictoc::toc()

tictoc::tic()
censo %>% count(provincia,comuna,dc,area,nviv)
tictoc::toc()


### conectar acciones 

DT[ ...
][ ...
][ ...
]

dt_viviendas[,hacinamiento := fifelse(p04 != 98,p04/cant_per,NA_real_,na= NA)][,mean(hacinamiento,na.rm = T),by=region][,mean(V1)]

dt_viviendas[,hacinamiento := fifelse(p04 != 98,p04/cant_per,NA_real_,na= NA)][
  ,pers_hog := fifelse(cant_hog != 98,cant_hog/cant_per,NA_real_,na= NA)]


## Concatenando acciones 

dt_esp <- dt_viviendas[region %in% c(1:10),][,c("area","comuna","cant_per")][,masde_4 := fifelse(cant_per > 4,1,0)]
dt_esp

dt_esp <- dt_viviendas[region %in% c(1:10),
                       ][,c("area","comuna","cant_per")
                         ][,masde_4 := fifelse(cant_per > 4,1,0)]
dt_esp






# Si por ejemplo queremos calcular el porcentaje en variables categóricas

### como lo hariamos con data.table

tictoc::tic()
dt_censo[,.N,by=comuna][
  ,Porc := round(100*N/sum(N),2)
][]
tictoc::toc()

### como lo hariamos con dplyr

tictoc::tic()
censo %>% 
  group_by(comuna) %>% 
  summarise(N = n()) %>% 
  mutate(Porc = round(100*N/sum(N),2))
tictoc::toc()

## y hacer un gráfico?

dt_censo[,{
  tot = .N
  .SD[,.(perc = round(100*.N/sum(tot),2)),by=p08 ]
},by=region][,barplot(perc,p08),by=region]



### joining data
# Creamos dos tablas una de conteo de viviendas por comuna y una de conteo de personas por comunas
viv_comuna <- dt_viviendas[,.N,by = .(comuna)]

pers_comuna <- dt_censo[,.N,by = comuna]

# Luego los unimos por el ID de las comunas
viv_comuna[pers_comuna, on = "comuna"]

pers_comuna[viv_comuna, on = "comuna"]


censo[vivienda[,c("id_manzana","area")], on = "id_manzana"]


dt_viviendas[,mean(cant_per, na.rm = T),.(comuna, nviv)]


# ejercico final

dt_viviendas[,.(personas = sum(cant_per)),by=.(comuna,region)][,.SD[which.max(personas)],by = region]

comunas <- read.csv2("C:/Users/Ricardo/Documents/INE/Clases R/Intermedio/data.table_intermedio/data/personas/etiquetas_persona_comuna_15r.csv")

dt_censo[,.(personas = .N),by=.(comuna,region)][,.SD[which.max(personas)],by = region][comunas, on = "comuna==valor", nomatch=NULL]



