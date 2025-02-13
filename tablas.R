# Librerías
library("dplyr")
library("gt")
library("gtExtras")
library("htmlwidgets")
library("plotly")
library("tidyverse")

path_png <- "C:/Users/parra/Documents/Tesis/Bases/resultados/gt_tablas"

# Elaborar las tablas GT y exportarlas en formato imagen #######################

# Variables utilizadas en la estimaci??n
names <- c("eda", 
           "sex", 
           "t_loc", 
           "ent", 
           "anios_esc", 
           "cs_p13_1", 
           "ing_x_hrs", 
           "emp_ppal")

description <- c("Edad",
                 "Sexo",
                 "Habitantes por localidad",
                 "Entidad",
                 "Años de escolaridad",
                 "Nivel escolar aprobado",
                 "Promedio de ingreso por hora trabajada",
                 "Empleo formal o informal de primera actividad")

tabla_variables <- data.frame(names, description)
colnames(tabla_variables) <- c("Nombre","Descripción")

tabla_variables_t <- tabla_variables %>% 
  gt() %>% 
  tab_header(title = md("**Variables Utilizadas**")) %>% 
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(columns=c(colnames(tabla_variables)))) %>% 
  tab_options(column_labels.background.color = "darkblue",
              table.margin.left = px(5),
              table.margin.right = px(5),
              data_row.padding = px(5))

# entidades cluster a??os de escolaridad promedio
colnames(entidades_cluster) <- c("Entidad","Rendimiento Promedio","Grupo")

entidades_cluster <- entidades_cluster %>% 
  arrange(desc(`Rendimiento Promedio`)) 

entidades_cluster_t <- entidades_cluster %>% 
  gt() %>% 
  tab_header(title = md("**Año de Escolaridad Adicional**")) %>% 
  fmt_percent(columns = "Rendimiento Promedio") %>% 
  tab_options(column_labels.background.color = "darkblue",
              table.margin.left = px(5),
              table.margin.right = px(5),
              data_row.padding = px(5)) %>% 
  gt_highlight_rows(rows = entidades_cluster$Grupo == "Alto",
                    fill = "green", 
                    alpha = 0.2) %>% 
  gt_highlight_rows(rows = entidades_cluster$Grupo == "Medio",
                    fill = "blue",
                    alpha = 0.2) %>% 
  gt_highlight_rows(rows = entidades_cluster$Grupo == "Bajo",
                    fill = "red",
                    alpha = 0.2)

# Mujer 
colnames(entidades_cluster_mujer) <- c("Entidad","Rendimiento Promedio","Grupo")

entidades_cluster_mujer <- entidades_cluster_mujer %>% 
  arrange(desc(`Rendimiento Promedio`)) 

entidades_cluster_mujer_t <- entidades_cluster_mujer %>% 
  gt() %>% 
  tab_header(title = md("**Brecha Salarial; Mujeres**")) %>% 
  fmt_percent(columns = "Rendimiento Promedio") %>% 
  tab_options(column_labels.background.color = "darkblue",
              table.margin.left = px(5),
              table.margin.right = px(5),
              data_row.padding = px(5)) %>% 
  gt_highlight_rows(rows = entidades_cluster_mujer$Grupo == "Baja",
                    fill = "green", 
                    alpha = 0.2) %>% 
  gt_highlight_rows(rows = entidades_cluster_mujer$Grupo == "Media",
                    fill = "blue",
                    alpha = 0.2) %>% 
  gt_highlight_rows(rows = entidades_cluster_mujer$Grupo == "Alta",
                    fill = "red",
                    alpha = 0.2) %>% 
  cols_align(align = "center")

# Urbano 
colnames(entidades_cluster_urbano) <- c("Entidad","Rendimiento Promedio","Grupo")

entidades_cluster_urbano <- entidades_cluster_urbano %>% 
  arrange(desc(`Rendimiento Promedio`)) 

entidades_cluster_urbano_t <- entidades_cluster_urbano %>% 
  gt() %>% 
  tab_header(title = md("**Brecha Salarial; Localidad Urbana**")) %>% 
  fmt_percent(columns = "Rendimiento Promedio") %>% 
  tab_options(column_labels.background.color = "darkblue",
              table.margin.left = px(5),
              table.margin.right = px(5),
              data_row.padding = px(5)) %>% 
  gt_highlight_rows(rows = entidades_cluster_urbano$Grupo == "Baja",
                    fill = "green", 
                    alpha = 0.2) %>% 
  gt_highlight_rows(rows = entidades_cluster_urbano$Grupo == "Media",
                    fill = "blue",
                    alpha = 0.2) %>% 
  gt_highlight_rows(rows = entidades_cluster_urbano$Grupo == "Alta",
                    fill = "red",
                    alpha = 0.2) %>% 
  cols_align(align = "center") 

# Formal
colnames(entidades_cluster_formal) <- c("Entidad","Rendimiento Promedio","Grupo")

entidades_cluster_formal <- entidades_cluster_formal %>% 
  arrange(desc(`Rendimiento Promedio`)) 

entidades_cluster_formal_t <- entidades_cluster_formal %>% 
  gt() %>% 
  tab_header(title = md("**Brecha Salarial; Trabajo Formal**")) %>% 
  fmt_percent(columns = "Rendimiento Promedio") %>% 
  tab_options(column_labels.background.color = "darkblue",
              table.margin.left = px(5),
              table.margin.right = px(5),
              data_row.padding = px(5)) %>% 
  gt_highlight_rows(rows = entidades_cluster_formal$Grupo == "Baja",
                    fill = "green", 
                    alpha = 0.2) %>% 
  gt_highlight_rows(rows = entidades_cluster_formal$Grupo == "Media",
                    fill = "blue",
                    alpha = 0.2) %>% 
  gt_highlight_rows(rows = entidades_cluster_formal$Grupo == "Alta",
                    fill = "red",
                    alpha = 0.2) %>% 
  cols_align(align = "center")

# Apendice
colnames(years) <- gsub("_bse","_ee",colnames(years))


years_tabla <- years %>% 
  select(c(sort(colnames(years)[colnames(years) != "Fecha"]))) %>%
  t() 

colnames(years_tabla) <- c(as.character(years$Fecha))

años <- format(unique(years$Fecha),"%Y")

# Asumiendo que 'years' es un vector con los años de interés
subtablas_list <- list()

for (year in años) {
  # Seleccionar columnas que contienen el año actual en el bucle
  cols <- grep(year, colnames(years_tabla), value = TRUE)
  
  # Crear una subtabla para el año actual
  subtabla <- years_tabla[, cols, drop = FALSE]
  
  subtabla <- data.frame(round(subtabla, 4))
  
  # Paso 1: Corregir los nombres de las columnas para el formato de fecha correcto
  nombres_corregidos <- gsub("X([0-9]{4})\\.([0-9]{2})\\.([0-9]{2})", "\\1-\\2-\\3", colnames(subtabla))
  colnames(subtabla) <- nombres_corregidos
  
  subtabla$nombres <- rownames(subtabla)
  
  orden_especifico <- c("Intercept_params","Intercept_ee",
                        "Años_coef","Años_ee",
                        "Experiencia_coef","Experiencia_ee",
                        "mujer_coef", "mujer_ee",
                        "formal_coef","formal_ee",
                        "urbano_coef","urbano_ee",
                        "experiencia_2_coef","experiencia_2_ee",
                        "R2"
                        )
  
  # Convertir la columna `nombres` a un factor con los niveles en el orden específico
  subtabla$nombres <- factor(subtabla$nombres, levels = orden_especifico)
  
  # Ordenar el DataFrame por la columna `nombres`
  subtabla <- subtabla[order(subtabla$nombres), ]
  
  # Formato
  subtabla <- subtabla %>% 
    select(nombres, colnames(subtabla)[colnames(subtabla) != "nombres"])
  
  rownames(subtabla) <- NULL
  
  # Opcional: Convertir la subtabla en una tabla 'gt' para formatear
  tabla_formateada <- gt(subtabla) %>%
    tab_header(title = paste("Resultados para el año",year))
  
  # Almacenar la tabla formateada en la lista
  subtablas_list[[year]] <- tabla_formateada
}

# Asumiendo que 'years' es un vector con los años de interés
colnames(levels) <- gsub("_bse","_ee",colnames(levels))

levels_tabla <- levels %>% 
  select(c(sort(colnames(levels)[colnames(levels) != "Fecha"]))) %>%
  t()

colnames(levels_tabla) <- c(as.character(levels$Fecha))


# Asumiendo que 'years' es un vector con los años de interés
subtablas_list_2 <- list()

for (year in años) {
  # Seleccionar columnas que contienen el año actual en el bucle
  cols <- grep(year, colnames(levels_tabla), value = TRUE)
  
  # Crear una subtabla para el año actual
  subtabla <- levels_tabla[, cols, drop = FALSE]
  
  subtabla <- data.frame(round(subtabla, 4))
  
  # Paso 1: Corregir los nombres de las columnas para el formato de fecha correcto
  nombres_corregidos <- gsub("X([0-9]{4})\\.([0-9]{2})\\.([0-9]{2})", "\\1-\\2-\\3", colnames(subtabla))
  colnames(subtabla) <- nombres_corregidos
  
  subtabla$nombres <- rownames(subtabla)
  
  orden_especifico <- c("Intercept_params", "Intercept_ee",
                        "Doctorado_coef", "Doctorado_ee",
                        "Maestría_coef", "Maestría_ee",
                        "Profesional_coef", "Profesional_ee",
                        "Preparatoria_coef", "Preparatoria_ee", 
                        "Secundaria_coef", "Secundaria_ee",
                        "Primaria_coef", "Primaria_ee",
                        "exp_params", "exp_ee",
                        "Mujer_coef", "Mujer_ee",
                        "Formal_coef", "Formal_ee",
                        "Urbano_coef", "Urbano_ee",
                        "np.square.exp._params", "np.square.exp._ee", 
                        "R2")
  
  # Convertir la columna `nombres` a un factor con los niveles en el orden específico
  subtabla$nombres <- factor(subtabla$nombres, levels = orden_especifico)
  
  # Ordenar el DataFrame por la columna `nombres`
  subtabla <- subtabla[order(subtabla$nombres), ]
  
  # Formato
  subtabla <- subtabla %>% 
    select(nombres, colnames(subtabla)[colnames(subtabla) != "nombres"])
  
  rownames(subtabla) <- NULL
  
  # Opcional: Convertir la subtabla en una tabla 'gt' para formatear
  tabla_formateada <- gt(subtabla) %>%
    tab_header(title = paste("Resultados para el año",year))
  
  # Almacenar la tabla formateada en la lista
  subtablas_list_2[[year]] <- tabla_formateada
}


# Crear una tabla resúmen del índice de GINI
# Elaborar un gráfco con el índice de GINI
### Importar los datos
data <- readxl::read_excel("C:/Users/parra/Documents/Tesis/Bases/indice_gini.xlsx") 

# Agregar una columna Fecha
data <- data %>%
  mutate(
    Mes = case_when(
      Trimestre == 1 ~ 1,
      Trimestre == 2 ~ 4,
      Trimestre == 3 ~ 7,
      Trimestre == 4 ~ 10
    ),
    Fecha = as.Date(paste(Año, Mes, "01", sep = "-"))
  )

gini_sum <- data %>%
  group_by(Nivel_Educativo) %>%
  summarise(Promedio = mean(Gini, na.rm = TRUE),
            sd = sd(Gini, na.rm = TRUE),
            coef_var = sd(Gini, na.rm = TRUE)/mean(Gini, na.rm = TRUE),
            max = max(Gini, na.rm = TRUE),
            min = min(Gini, na.rm = TRUE)) %>%
  arrange(desc(Promedio))

gini_sum_t <- gini_sum %>%
  gt() %>%
  tab_header(title = md("**Indice de Gini por Nivel Educativo, México**")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_options(
    column_labels.background.color = "darkblue",
    table.margin.left = px(5),
    table.margin.right = px(5),
    data_row.padding = px(5),
    table.font.size = px(9)
  ) %>%
  # Redondear desde la columna 2 en adelante
  fmt_number(
    columns = 2:ncol(gini_sum),
    decimals = 4
  ) %>%
  # Centrar desde la columna 2 en adelante
  cols_align(
    align = "center",
    columns = 2:ncol(gini_sum)
  )

# Importar la tabla de ingreso promedio del último trimestre del 2023
ing_prom_ht <- readxl::read_excel("C:/Users/parra/Documents/Tesis/Bases/media_condicional.xlsx",
                           sheet = 1) %>%
  data.frame()
colnames(ing_prom_ht)[1] <- "Educación"

# Elaborar tablas 
ing_prom_ht_gt <- ing_prom_ht %>%
  gt() %>%
  tab_header(title = md("**Ingreso Promedio por Hora Trabajada: Q4 2023**")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_options(
    column_labels.background.color = "darkblue",
    table.margin.left = px(5),
    table.margin.right = px(5),
    data_row.padding = px(5),
    table.font.size = px(9)
  ) %>%
  fmt_currency(columns = 2:ncol(ing_prom_ht),
               decimals = 2) %>%
  # Centrar desde la columna 2 en adelante
  cols_align(
    align = "center",
    columns = 2:ncol(gini_sum)
  )



# Importar la tabla de horas promedio trabajadas, del último trimestre del 2023.
hrs_prom_ht <- readxl::read_excel("C:/Users/parra/Documents/Tesis/Bases/media_condicional.xlsx",
                                  sheet = 2) 
colnames(hrs_prom_ht)[1] <- "Educación"

# Elaborar tablas 
hrs_prom_ht_gt <- hrs_prom_ht %>%
  gt() %>%
  tab_header(title = md("**Horas Promedio Trabajadas por Semana: Q4 2023**")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_options(
    column_labels.background.color = "darkblue",
    table.margin.left = px(5),
    table.margin.right = px(5),
    data_row.padding = px(5),
    table.font.size = px(9)
  ) %>%
  fmt_number(columns = 2:ncol(hrs_prom_ht),
               decimals = 2) %>%
  # Centrar desde la columna 2 en adelante
  cols_align(
    align = "center",
    columns = 2:ncol(gini_sum)
  )


# Relacion EDUCACION PADRE - HIJOS 
# ====================================================================
concentrado_hogar <- read.csv("C:/Users/parra/Documents/Tesis/Bases/conjunto_de_datos_enigh_ns_2022_csv/conjunto_de_datos_concentradohogar_enigh2022_ns/conjunto_de_datos/conjunto_de_datos_concentradohogar_enigh2022_ns.csv")
poblacion <- read.csv("C:/Users/parra/Documents/Tesis/Bases/conjunto_de_datos_enigh_ns_2022_csv/conjunto_de_datos_poblacion_enigh2022_ns/conjunto_de_datos/conjunto_de_datos_poblacion_enigh2022_ns.csv")

# Unir las tablas de población y vivienda
global <- merge(concentrado_hogar, poblacion, by = c("folioviv","foliohog"))

# Seleccionar las variables de interes
global_filtrado <- global %>%
  dplyr::select(all_of(c("folioviv",
                         "educa_jefe",
                         "educacion",
                         "parentesco",
                         "sexo",
                         "edad",
                         "antec_esc",
                         "tipoesc"))) %>%
  mutate(across(everything(), as.numeric))

# Agregar niveles
gf_niveles <- global_filtrado %>%
  mutate(
    parentesco = case_when(
      parentesco == 101 ~ "Jefe", 
      parentesco == 102 ~ "Esposo",
      parentesco %in% c(301, 302, 303, 304, 305) ~ "Hijo(a)",
      parentesco == 609 ~ "Nieto(a)",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    tipoesc = case_when(tipoesc %in% c(1) ~ "Publica",
                        tipoesc %in% c(2) ~ "Privada",
                        TRUE ~ NA_character_
                        )
    ) %>%
  mutate(
    antec_esc = case_when(antec_esc == 1 ~ "Primaria",
                          antec_esc == 2 ~ "Secundaria",
                          antec_esc == 3 ~ "Preparatoria",
                          antec_esc == 4 ~ "Licenciatura",
                          antec_esc == 5 ~ "Maestria",
                          TRUE ~ NA_character_
                          )
  ) %>%
  mutate(
    educa_jefe = case_when(
      educa_jefe == 11 ~ "Posgrado",
      educa_jefe == 10 ~ "Profesional completa",
      educa_jefe == 9 ~ "Profesional incompleta",
      educa_jefe == 8 ~ "Preparatoria completa",
      educa_jefe == 7 ~ "Preparatoria incompleta",
      educa_jefe == 6 ~ "Secundaria completa",
      educa_jefe == 5 ~ "Secundaria incompleta",
      educa_jefe == 4 ~ "Primaria completa",
      educa_jefe == 3 ~ "Primaria incompleta",
      educa_jefe == 2 ~ "Preescolar",
      educa_jefe == 1 ~ "Sin instruccion",
      TRUE ~ NA_character_
    )
  )

# Agrupar por tipo de educación
gf_n_resumido <- gf_niveles %>%
  filter(!is.na(educa_jefe) & !is.na(antec_esc)) %>%
  count(educa_jefe, antec_esc) %>%
  pivot_wider(names_from = antec_esc, values_from = n, values_fill = 0) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>%
  ungroup()

# Dar formato a la tabla
gf_n_resumido <- gf_n_resumido[c("educa_jefe","Maestria","Licenciatura","Preparatoria","Secundaria","Primaria","Total")]
data_labels <- data.frame(index = c(1,2,3,4,5,6,7,8,9,10,11) , 
                          educa_jefe = c("Posgrado",
                                    "Profesional completa",
                                    "Profesional incompleta",
                                    "Preparatoria completa",
                                    "Preparatoria incompleta",
                                    "Secundaria completa",
                                    "Secundaria incompleta",
                                    "Primaria completa",
                                    "Primaria incompleta",
                                    "Preescolar",
                                    "Sin instruccion")
                          )

gf_n_resumido <- merge(data_labels, gf_n_resumido, by = "educa_jefe")

gf_n_resumido <- gf_n_resumido %>%
  arrange(index) %>%
  select(-index)


# Definir niveles de educación en orden creciente
educa_jefe_niveles <- c("Sin instruccion", "Preescolar", "Primaria incompleta",
                        "Primaria completa", "Secundaria incompleta", "Secundaria completa",
                        "Preparatoria incompleta", "Preparatoria completa", 
                        "Profesional incompleta", "Profesional completa", "Posgrado")

# Convertir educa_jefe a numérico
gf_n_resumido <- gf_n_resumido %>%
  mutate(educa_jefe_num = match(educa_jefe, educa_jefe_niveles))  # Asigna valores numéricos

# Calcular la correlación de Pearson entre la educación del jefe y la educación de los hijos
correlaciones <- cor(gf_n_resumido %>% select(educa_jefe_num, Maestria, Licenciatura, Preparatoria, Secundaria, Primaria),
                     use = "complete.obs", method = "pearson")

# Mostrar correlaciones entre educa_jefe y cada nivel de educación de los hijos
correlaciones <- correlaciones[1, -1]  # Extraer solo las correlaciones con educa_jefe_num
corr_df <- data.frame(t(correlaciones))

cor_t <- corr_df %>%
  gt() %>%
  tab_header(title = md("**Coef. Cor. Ed. Padre vs Hijo**")) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_options(
    column_labels.background.color = "darkblue",
    table.margin.left = px(5),
    table.margin.right = px(5),
    data_row.padding = px(5),
    table.font.size = px(9)
  ) %>%
  fmt_number(columns = 1:ncol(corr_df),
             decimals = 4) %>%
  # Centrar desde la columna 2 en adelante
  cols_align(
    align = "center",
    columns = 1:ncol(corr_df)
  )

# Crear una tabla de contingencia
tabla_chi <- gf_n_resumido %>%
  select(-Total) %>%
  column_to_rownames(var = "educa_jefe") %>%
  as.matrix()

# Prueba de Chi-Cuadrado
chisq.test(tabla_chi)

chisq.test(tabla_chi, simulate.p.value = TRUE, B = 10000)
