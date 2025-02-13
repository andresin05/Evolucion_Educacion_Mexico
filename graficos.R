library("dplyr")
library("lubridate")
library("ggplot2")
library("tidyverse")
library("scales")
library("cluster")
library("factoextra")
library("sf")
library("gridExtra")

# Importat los datos

# Archivos
# ==============================================================================
path <- "C:/Users/parra/Documents/Tesis/Bases/resultados"
path_2 <- "C:/Users/parra/Documents/Tesis/Bases"
path_levels <- "C:/Users/parra/Documents/Tesis/Bases/resultados/Levels/" 
path_years <- "C:/Users/parra/Documents/Tesis/Bases/resultados/Years/"
path_mapa <- "C:/Users/parra/Documents/Tesis/Bases/Marco_geoestadistico/Entidades"

archivos <- list.files(path)
archivos_levels <- list.files(path_levels)
archivos_years <- list.files(path_years)
mapa_original <- st_read(file.path(path_mapa, "00ent.shp"))

# Estimaciones aC1os de educaciC3n
years <- read.csv(file.path(path, archivos[archivos %in% c("df_params_years.csv")])) %>% 
  mutate(Fecha = ymd(Fecha)) %>% 
  rename(mujer_coef = sex.T.Mujer._params,
         urbano_coef = t_loc.T.Urbano._params,
         formal_coef = emp_ppal.T.formal._params,
         Años_coef = anios_esc_params,
         Experiencia_coef = exp_params,
         experiencia_2_coef = np.square.exp._params) %>%
  rename(mujer_bse = sex.T.Mujer._bse,
         urbano_bse = t_loc.T.Urbano._bse,
         formal_bse = emp_ppal.T.formal._bse,
         Años_bse = anios_esc_bse,
         Experiencia_bse = exp_bse,
         experiencia_2_bse = np.square.exp._bse)

years_tabla <- years %>%
  dplyr::rename(Años = Años_coef,
                Experiencia = Experiencia_coef) %>%
  pivot_longer(cols = -Fecha, names_to = "regresor", values_to = "valor")

# Estimaciones por niveles de educaciC3n
levels <-  read.csv(file.path(path, archivos[archivos %in% c("df_params_levels.csv")])) %>% 
  mutate(Fecha = ymd(Fecha)) %>% 
  rename(Primaria_coef = cs_p13_1.T.Primaria._params,
         Secundaria_coef = cs_p13_1.T.Secundaria._params,
         Preparatoria_coef = cs_p13_1.T.Preparatoria.o.bachillerato._params,
         Profesional_coef = cs_p13_1.T.Profesional._params,
         Maestría_coef = cs_p13_1.T.Maestría._params,
         Doctorado_coef = cs_p13_1.T.Doctorado._params,
         Mujer_coef = sex.T.Mujer._params,
         Urbano_coef = t_loc.T.Urbano._params,
         Formal_coef = emp_ppal.T.formal._params) %>%
  rename(Primaria_bse = cs_p13_1.T.Primaria._bse,
         Secundaria_bse = cs_p13_1.T.Secundaria._bse,
         Preparatoria_bse = cs_p13_1.T.Preparatoria.o.bachillerato._bse,
         Profesional_bse = cs_p13_1.T.Profesional._bse,
         Maestría_bse = cs_p13_1.T.Maestría._bse,
         Doctorado_bse = cs_p13_1.T.Doctorado._bse,
         Mujer_bse = sex.T.Mujer._bse,
         Urbano_bse = t_loc.T.Urbano._bse,
         Formal_bse = emp_ppal.T.formal._bse)

# Estimaciones de años de educación por estado
years_list <- lapply(archivos_years, function(archivo) {
  # Construir la ruta completa del archivo
  ruta_completa <- file.path(path_years, archivo)
  
  nombre <- gsub("df_params_states_years_|\\.csv", "", archivo)
  
  # Leer el archivo CSV
  datos <- read.csv(ruta_completa) %>% 
    mutate(Fecha = ymd(Fecha)) %>% 
    rename(mujer = sex.T.Mujer.,
           urbano = t_loc.T.Urbano.,
           formal = emp_ppal.T.formal.,
           años = anios_esc,
           experiencia = exp,
           experiencia_2 = np.square.exp.) %>% 
    mutate(entidad = nombre)
  
  # Devolver los datos leC-dos
  return(datos)
})

# Data frame de aC1os de educaciC3n
years_list_df <- do.call(rbind, years_list) # Unir los data frames por filas

# Transformar en formato long
years_list_df_long <- years_list_df %>% 
  select(c("Fecha","entidad","años","experiencia")) %>% # Seleccionar la fecha, la entidad, los aC1os y la experiencia
  pivot_longer(cols = -c("Fecha","entidad"), values_to = "coeficiente", names_to = "variable")

# Estimaciones de niveles de educaciCón por estado
levels_list <- lapply(archivos_levels, function(archivo) {
  # Construir la ruta completa del archivo
  ruta_completa <- file.path(path_levels, archivo)
  
  nombre <- gsub("df_params_states_levels_|\\.csv", "", archivo)
  
  # Leer el archivo CSV
  datos <- read.csv(ruta_completa) %>% 
    mutate(Fecha = ymd(Fecha)) %>% 
    rename(primaria = cs_p13_1.T.Primaria.,
           secundaria = cs_p13_1.T.Secundaria.,
           preparatoria = cs_p13_1.T.Preparatoria.o.bachillerato.,
           profesional = cs_p13_1.T.Profesional.,
           maestría = cs_p13_1.T.Maestría.,
           doctorado = cs_p13_1.T.Doctorado.,
           mujer = sex.T.Mujer.,
           urbano = t_loc.T.Urbano.,
           formal = emp_ppal.T.formal.) %>% 
    mutate(entidad = nombre)
  
  # Devolver los datos leC-dos
  return(datos)
})

# Cálculo de los clústers 
# ==============================================================================
# Agrupar por entidad y calcular el promedio del coeficiente para cada entidad
entidades_cluster <- years_list_df_long %>%
  filter(variable == "años") %>%
  group_by(entidad) %>%
  summarise(coeficiente_promedio = mean(coeficiente)) %>%
  ungroup()

# Ejemplo utilizando kmeans
wcss <- numeric(length = 10)  # Supongamos que estamos probando hasta 10 clC:sters

for (i in 1:10) {
  kmeans_model <- kmeans(entidades_cluster[, 2, drop = FALSE], centers = i)
  wcss[i] <- kmeans_model$tot.withinss
}

# Crear un marco de datos con los resultados
elbow_data <- data.frame(Num_Clusters = 1:10, WCSS = wcss)

# Crear el grC!fico de codo con ggplot2
clusters_plot <- ggplot(elbow_data, aes(x = Num_Clusters, y = WCSS)) +
  geom_line(color = "#1F78B4", linewidth = 1.2) +  # LC-nea suave en azul
  geom_point(color = "#E31A1C", size = 3) +  # Puntos en rojo mC!s grandes
  labs(title = "Gráfico de Codo",
       x = "Número de Clústeres",
       y = "WCSS",
       caption = "AnC!lisis de K-means") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#CCCCCC", linetype = "dashed"),  # LC-neas de cuadrC-cula punteadas
        panel.background = element_rect(fill = "#F8F8F8"),  # Fondo del grC!fico mC!s claro
        plot.title =  element_text(color = "black", size = 15, face = "bold", hjust = 0.5),  # TC-tulo en negro y negrita
        plot.caption = element_text(color = "#666666", size = 12, face = "bold"),
        axis.title = element_text(color = "#555555", size = 16),  # Etiquetas de ejes mC!s oscuras
        axis.text = element_text(color = "#555555", size = 16))  # NC:meros de ejes mC!s oscuros

# Realizar análisis de clúster
k <- 3  # NC:mero de clusters, ajusta segC:n tus necesidades
set.seed(123)  # Establecer semilla para reproducibilidad
entidades_cluster$cluster <- kmeans(entidades_cluster[, 2, drop = FALSE], centers = k)$cluster

# Fusionar la informaciC3n de clC:ster con years_list_df_long
years_list_df_long <- left_join(years_list_df_long, entidades_cluster, by = "entidad") %>% 
  mutate(cluster = ifelse(cluster == 1, "Alto", 
                          ifelse(cluster == 3, "Medio",
                                 ifelse(cluster == 2, "Bajo", NA)
                          )
  )
  )

# Transformar los labels como Alto, Medio, Bajo.
entidades_cluster <- entidades_cluster %>% 
  mutate(cluster = ifelse(cluster == 1, "Alto", 
                          ifelse(cluster == 3, "Medio",
                                 ifelse(cluster == 2, "Bajo", NA)
                          )
  )
  )

# Mapa de los aC1os de educación
mapa_years <- merge(mapa_original, entidades_cluster, by.x = "NOMGEO", by.y = "entidad", all.x = TRUE)

mapa_years_plot <- ggplot() +
  geom_sf(data = mapa_years, aes(fill = factor(cluster, levels = c("Bajo","Medio","Alto"))), color = "black", lwd = 0.2) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Rendimiento (Años) Promedio por Grupo:  Q1/2005 - Q2/2023",
       fill = "Grupo",
       caption = "ENOE") +
  theme_minimal() +
  theme_void() +
  theme(plot.title =  element_text(color = "black", size = 24, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "#666666", size = 24, face = "bold", hjust = 0.5),
        legend.title = element_text(color = "#666666", size = 24, face = "bold", hjust = 0.5),
        legend.text = element_text(color = "#666666", size = 15))

# Crear el gráfico de dispersiòn
plot_years_entidad <- years_list_df_long %>% 
  filter(variable == "años") %>% 
  ggplot(aes(x = Fecha, y = coeficiente, color = as.factor(cluster))) +
  geom_point(alpha = 0.3, size = 3) +  # Ajustar la transparencia de los puntos
  scale_color_manual(values = c("#1F78B4", "#33A02C", "#E31A1C")) +  # Personalizar colores para tres clC:sters
  theme_minimal() +
  theme(legend.position = "bottom") + # Mover la leyenda a la parte inferior
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Gráfico de Clústers por Año y Entidad", 
       x = " ", y = " ",
       caption = "ENOE") +
  theme(
    title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),  # Centra el tC-tulo
    plot.caption = element_text(color = "#666666", size = 15, face = "bold"),
    axis.text = element_text(color = "#666666", size = 15),
    axis.title = element_text(color = "#666666", size = 15),
    legend.title = element_blank(),
    legend.text = element_text(color = "#666666", size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.border = element_blank(),
    legend.position = "bottom")


# Plot years
plot_years <- years %>% 
  select("Fecha", "Años_coef", "Experiencia_coef") %>% 
  rename(Años = Años_coef,
         Experiencia = Experiencia_coef) %>%
  pivot_longer(cols = -"Fecha", values_to = "coeficiente", names_to = "variable") %>% 
  ggplot(aes(x = Fecha, y = coeficiente, color = variable)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = 0, ymax = coeficiente, fill = variable), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Rendimientos: Q1/2005 - Q4/2023",
       y = " ", x = " ",
       caption = "ENOE") +
  theme(
    title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),  # Centra el tC-tulo
    plot.caption = element_text(color = "#666666", size = 16, face = "bold"),
    axis.text = element_text(color = "#666666", size = 15),
    axis.title = element_text(color = "#666666", size = 15),
    legend.title = element_blank(),
    legend.text = element_text(color = "#666666", size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.border = element_blank(),
    legend.position = "bottom") +
  geom_point(data =  years %>% 
               select("Fecha", "Años_coef", "Experiencia_coef") %>% 
               rename(Años = Años_coef,
                      Experiencia = Experiencia_coef) %>% 
               pivot_longer(cols = -"Fecha", values_to = "coeficiente", names_to = "variable") %>%
               filter(Fecha %in% as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(years$Fecha),"%Y-%m-%d")))),
             aes(x = Fecha, y = coeficiente, color = variable, shape = variable), size = 3) +
  geom_text(data = years %>% 
              select("Fecha", "Años_coef", "Experiencia_coef") %>% 
              rename(Años = Años_coef,
                     Experiencia = Experiencia_coef) %>% 
              pivot_longer(cols = -"Fecha", values_to = "coeficiente", names_to = "variable") %>%
              filter(Fecha %in% as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(years$Fecha),"%Y-%m-%d")))),
            aes(x = Fecha, y = coeficiente, label = scales::percent(coeficiente), fontface = "bold"), vjust = -0.5, hjust = 0.5, color = "black", size = 4) +
  scale_y_continuous(labels = scales::percent_format())

# Niveles de educaciC3n
plot_levels <- levels %>% 
  select(all_of(c("Fecha",
                  "Primaria_coef",                    
                  "Secundaria_coef",
                  "Preparatoria_coef",
                  "Profesional_coef",
                  "Maestría_coef",
                  "Doctorado_coef"))) %>% 
  rename(Primaria = Primaria_coef,
         Secundaria = Secundaria_coef,
         Preparatoria = Preparatoria_coef,
         Profesional = Profesional_coef,
         Maestrìa = Maestría_coef,
         Doctorado = Doctorado_coef) %>%
  pivot_longer(cols = -"Fecha", values_to = "coeficiente", names_to = "variable") %>% 
  ggplot(aes(x = Fecha, y = coeficiente, color = variable)) +
  geom_line(size = 1.5) +
  geom_ribbon(aes(ymin = 0, ymax = coeficiente, fill = variable), alpha = 0.2) +
  theme_minimal() +
  labs(title = "Rendimientos por Nivel Educativo: Q1/2005 - Q4/2023",
       y = " ", x = " ",
       caption = "ENOE") +
  theme(
    title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),  # Centra el tC-tulo
    plot.caption = element_text(color = "#666666", size = 16, face = "bold"),
    axis.text = element_text(color = "#666666", size = 15),
    axis.title = element_text(color = "#666666", size = 15),
    legend.title = element_blank(),
    legend.text = element_text(color = "#666666", size = 15),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.border = element_blank(),
    legend.position = "bottom") +
  geom_point(data =  levels %>% 
               select(all_of(c("Fecha",
                               "Primaria_coef",                    
                               "Secundaria_coef",
                               "Preparatoria_coef",
                               "Profesional_coef",
                               "Maestría_coef",
                               "Doctorado_coef"))) %>% 
               rename(Primaria = Primaria_coef,
                      Secundaria = Secundaria_coef,
                      Preparatoria = Preparatoria_coef,
                      Profesional = Profesional_coef,
                      Maestrìa = Maestría_coef,
                      Doctorado = Doctorado_coef) %>%
               pivot_longer(cols = -"Fecha", values_to = "coeficiente", names_to = "variable") %>% 
               filter(Fecha %in% as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(years$Fecha),"%Y-%m-%d")))),
             aes(x = Fecha, y = coeficiente, color = variable, shape = variable), size = 3) +
  geom_text(data =  levels %>% 
              select(all_of(c("Fecha",
                              "Primaria_coef",                    
                              "Secundaria_coef",
                              "Preparatoria_coef",
                              "Profesional_coef",
                              "Maestría_coef",
                              "Doctorado_coef"))) %>% 
              rename(Primaria = Primaria_coef,
                     Secundaria = Secundaria_coef,
                     Preparatoria = Preparatoria_coef,
                     Profesional = Profesional_coef,
                     Maestrìa = Maestría_coef,
                     Doctorado = Doctorado_coef) %>% 
              pivot_longer(cols = -"Fecha", values_to = "coeficiente", names_to = "variable") %>%
              filter(Fecha %in% as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(years$Fecha),"%Y-%m-%d")))),
            aes(x = Fecha, y = coeficiente, label = scales::percent(coeficiente), fontface = "bold"), vjust = -0.5, hjust = 0.5, color = "black", size = 4) +
  scale_y_continuous(labels = scales::percent_format())


# Mujer
plot_woman <- years %>% 
  select(c("mujer_coef","Fecha")) %>% 
  rename(mujer = mujer_coef) %>%
  ggplot(aes(x = Fecha, y = mujer)) +
  geom_line(linewidth = 1.5, color = "seagreen") +
  geom_ribbon(aes(ymin = 0, ymax = mujer), fill = "darkgreen", alpha = 0.5) +  # Cambia el color del fill aquC-
  theme_minimal() +
  labs(title = "Brecha Salarial: Mujeres",
       y = " ", x = " ",
       caption = "ENOE") +
  theme(
    title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),  # Centra el tC-tulo
    plot.caption = element_text(color = "#666666", size = 16, face = "bold"),
    axis.text = element_text(color = "#666666", size = 15),
    axis.title = element_text(color = "#666666", size = 15),
    legend.title = element_blank(),
    legend.text = element_text(color = "#666666"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.border = element_blank(),
    legend.position = "bottom") +
  geom_point(data = years %>% 
               select(c("mujer_coef","Fecha")) %>% 
               rename(mujer = mujer_coef) %>% 
               filter(Fecha %in% as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(years$Fecha),"%Y-%m-%d")))),
             aes(x = Fecha, y = mujer), color = "darkgreen" , size = 3) +
  geom_text(data = years %>% 
              select(c("mujer_coef","Fecha")) %>% 
              rename(mujer = mujer_coef) %>%
              filter(Fecha %in% as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(years$Fecha),"%Y-%m-%d")))),
            aes(x = Fecha, y = mujer, label = scales::percent(mujer), fontface = "bold"), vjust = -0.5, hjust = 0.5, color = "black", size = 4) +
  scale_y_continuous(labels = scales::percent_format())

# Informal
plot_informal <- years %>% 
  select(c("formal_coef","Fecha")) %>% 
  rename(formal = formal_coef) %>%
  ggplot(aes(x = Fecha, y = formal)) +
  geom_line(size = 1.5, color = "royalblue") +
  geom_ribbon(aes(ymin = 0, ymax = formal), fill = "royalblue", alpha = 0.5) +  # Cambia el color del fill aquC-
  theme_minimal() +
  labs(title = "Brecha Salarial: Empleo Formal",
       y = " ", x = " ",
       caption = "ENOE") +
  theme(
    title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),  # Centra el tC-tulo
    plot.caption = element_text(color = "#666666", size = 16, face = "bold"),
    axis.text = element_text(color = "#666666", size = 15),
    axis.title = element_text(color = "#666666", size = 15),
    legend.title = element_blank(),
    legend.text = element_text(color = "#666666"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.border = element_blank(),
    legend.position = "bottom") +
  geom_point(data = years %>% 
               select(c("formal_coef","Fecha")) %>% 
               rename(formal = formal_coef) %>% 
               filter(Fecha %in% as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(years$Fecha),"%Y-%m-%d")))),
             aes(x = Fecha, y = formal), color = "royalblue" , size = 3) +
  geom_text(data = years %>% 
              select(c("formal_coef","Fecha")) %>% 
              rename(formal = formal_coef) %>%
              filter(Fecha %in% as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(years$Fecha),"%Y-%m-%d")))),
            aes(x = Fecha, y = formal, label = scales::percent(round(formal,4)), fontface = "bold"), vjust = -0.5, hjust = 0.5, color = "black", size = 4) +
  scale_y_continuous(labels = scales::percent_format())

# Urbano
plot_urbano <- years %>% 
  select(c("urbano_coef","Fecha")) %>% 
  rename(urbano = urbano_coef) %>%
  ggplot(aes(x = Fecha, y = urbano)) +
  geom_line(size = 1.5, color = "snow3") +
  geom_ribbon(aes(ymin = 0, ymax = urbano), fill = "seashell4", alpha = 0.5) +  # Cambia el color del fill aquC-
  theme_minimal() +
  labs(title = "Brecha Salarial; Localidad Urbana",
       y = " ", x = " ",
       caption = "ENOE") +
  theme(
    title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5),  # Centra el tC-tulo
    plot.caption = element_text(color = "#666666", size = 16, face = "bold"),
    axis.text = element_text(color = "#666666", size = 15),
    axis.title = element_text(color = "#666666", size = 15),
    legend.title = element_blank(),
    legend.text = element_text(color = "#666666"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.background = element_rect(fill = "#FFFFFF"),
    plot.background = element_rect(fill = "#FFFFFF"),
    panel.border = element_blank(),
    legend.position = "bottom") +
  geom_point(data = years %>% 
               select(c("urbano_coef","Fecha")) %>% 
               rename(urbano = urbano_coef) %>% 
               filter(Fecha %in% as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(years$Fecha),"%Y-%m-%d")))),
             aes(x = Fecha, y = urbano), color = "snow3" , size = 3) +
  geom_text(data = years %>% 
              select(c("urbano_coef","Fecha")) %>% 
              rename(urbano = urbano_coef) %>%
              filter(Fecha %in% as.Date(c("2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(years$Fecha),"%Y-%m-%d")))),
            aes(x = Fecha, y = urbano, label = scales::percent(urbano), fontface = "bold"), vjust = -0.5, hjust = 0.5, color = "black", size = 4) +
  scale_y_continuous(labels = scales::percent_format())

# Graficos mapas variables extra a la ecuaciòn de Mincer

# Mujer
mujer_list_df_long <- years_list_df %>% 
  select(c("Fecha","entidad","mujer")) %>% 
  pivot_longer(cols = -c("Fecha","entidad"), values_to = "coeficiente", names_to = "variable")

# Agrupar por entidad y calcular el promedio del coeficiente para cada entidad
entidades_cluster_mujer <- mujer_list_df_long %>%
  group_by(entidad) %>%
  summarise(coeficiente_promedio = mean(coeficiente)) %>%
  ungroup()

# Realizar anC!lisis de clC:ster
k <- 3  # NC:mero de clusters, ajusta segC:n tus necesidades
set.seed(123)  # Establecer semilla para reproducibilidad
entidades_cluster_mujer$cluster <- kmeans(entidades_cluster_mujer[, 2, drop = FALSE], centers = k)$cluster

# Reasignar los labels
entidades_cluster_mujer <- entidades_cluster_mujer %>% 
  mutate(cluster = ifelse(cluster == 1, "Media", 
                          ifelse(cluster == 2, "Alta",
                                 ifelse(cluster == 3, "Baja", NA)
                          )
  )
  ) 

# Mapa
mapa_mujer <- merge(mapa_original, entidades_cluster_mujer, by.x = "NOMGEO", by.y = "entidad", all.x = TRUE)

mapa_plot_mujer <- ggplot() +
  geom_sf(data = mapa_mujer, aes(fill = factor(cluster, levels = c("Alta","Media","Baja"))), color = "black", lwd = 0.2) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Brecha Salarial Promedio: Mujeres",
       caption = "ENOE",
       fill = "Grupo") +
  theme_minimal() +
  theme_void() +
  theme(plot.title =  element_text(color = "black", size = 24, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "#666666", size = 24, face = "bold", hjust = 0.5),
        legend.title = element_text(color = "#666666", size = 24, face = "bold", hjust = 0.5),
        legend.text = element_text(color = "#666666", size = 15))

# Urbano
urbano_list_df_long <- years_list_df %>% 
  select(c("Fecha","entidad","urbano")) %>% 
  pivot_longer(cols = -c("Fecha","entidad"), values_to = "coeficiente", names_to = "variable")

# Agrupar por entidad y calcular el promedio del coeficiente para cada entidad
entidades_cluster_urbano <- urbano_list_df_long %>%
  group_by(entidad) %>%
  summarise(coeficiente_promedio = mean(coeficiente)) %>%
  ungroup()

# Realizar anC!lisis de clC:ster
k <- 3  # NC:mero de clusters, ajusta segC:n tus necesidades
set.seed(123)  # Establecer semilla para reproducibilidad
entidades_cluster_urbano$cluster <- kmeans(entidades_cluster_urbano[, 2, drop = FALSE], centers = k)$cluster

# Reasignar los labels
entidades_cluster_urbano <- entidades_cluster_urbano %>% 
  mutate(cluster = ifelse(cluster == 1, "Alta", 
                          ifelse(cluster == 3, "Media",
                                 ifelse(cluster == 2, "Baja", NA)
                          )
  )
  ) 

# Mapa
mapa_urbano <- merge(mapa_original, entidades_cluster_urbano, by.x = "NOMGEO", by.y = "entidad", all.x = TRUE)

mapa_plot_urbano <- ggplot() +
  geom_sf(data = mapa_urbano, aes(fill = factor(cluster, levels =  c("Alta","Media","Baja"))), color = "black", lwd = 0.2) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Brecha Salarial Promedio: Localidad Urbana",
       caption = "ENOE",
       fill = "Grupo") +
  theme_minimal() +
  theme_void() +
  theme(plot.title =  element_text(color = "black", size = 24, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "#666666", size = 24, face = "bold", hjust = 0.5),
        legend.title = element_text(color = "#666666", size = 24, face = "bold", hjust = 0.5),
        legend.text = element_text(color = "#666666", size = 15))

# Formal
formal_list_df_long <- years_list_df %>% 
  select(c("Fecha","entidad","formal")) %>% 
  pivot_longer(cols = -c("Fecha","entidad"), values_to = "coeficiente", names_to = "variable")

# Agrupar por entidad y calcular el promedio del coeficiente para cada entidad
entidades_cluster_formal <- formal_list_df_long %>%
  group_by(entidad) %>%
  summarise(coeficiente_promedio = mean(coeficiente)) %>%
  ungroup()

# Realizar anC!lisis de clC:ster
k <- 3  # NC:mero de clusters, ajusta segC:n tus necesidades
set.seed(123)  # Establecer semilla para reproducibilidad
entidades_cluster_formal$cluster <- kmeans(entidades_cluster_formal[, 2, drop = FALSE], centers = k)$cluster

# Reasignar los labels
entidades_cluster_formal <- entidades_cluster_formal %>% 
  mutate(cluster = ifelse(cluster == 2, "Alta", 
                          ifelse(cluster == 1, "Media",
                                 ifelse(cluster == 3, "Baja", NA)
                          )
  )
  ) 

# Mapa
mapa_formal <- merge(mapa_original, entidades_cluster_formal, by.x = "NOMGEO", by.y = "entidad", all.x = TRUE)

mapa_plot_formal <- ggplot() +
  geom_sf(data = mapa_formal, aes(fill = factor(cluster, levels = c("Alta","Media","Baja"))), color = "black", lwd = 0.2) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Brecha Salarial: Empleo Formal",
       caption = "ENOE",
       fill = "Grupo") +
  theme_minimal() +
  theme_void() +
  theme(plot.title =  element_text(color = "black", size = 24, face = "bold", hjust = 0.5),
        plot.caption = element_text(color = "#666666", size = 24, face = "bold", hjust = 0.5),
        legend.title = element_text(color = "#666666", size = 24, face = "bold", hjust = 0.5),
        legend.text = element_text(color = "#666666", size = 15))

# Grafico Global
# ==============================================================================
data <- read.csv(file.path(path_2, "Data.csv"))

colnames(data) <- gsub(".*YR(\\d+).*", "\\1", colnames(data))

# Paso 1: Seleccionar y transformar los datos
data_long <- data %>%
  dplyr::select(Country.Name, Series.Name, `2011`:`2018`) %>%
  pivot_longer(cols = `2011`:`2018`, names_to = "Year", values_to = "Value") %>%
  mutate(Year = as.numeric(Year), Value = as.numeric(Value))

# Paso 2: Filtrar los datos según las series de interés
a <- data_long %>%
  filter(Series.Name == "UIS: Mean years of schooling (ISCED 1 or higher), population 25+ years, both sexes") %>%
  select(Country.Name, Year, Schooling = Value)

b <- data_long %>%
  filter(Series.Name == "GDP per capita (current US$)") %>%
  select(Country.Name, Year, GDP = Value)

# Paso 3: Unir las dos series por país y año
total <- inner_join(a, b, by = c("Country.Name", "Year")) %>%
  na.omit()

# Paso 4: Graficar los datos y etiquetar los puntos de México
global_plot <- ggplot(total, aes(x = Schooling, y = log(GDP))) +
  geom_point(color = "darkblue", size = 2) +
  geom_point(data = total %>% filter(Country.Name == "Mexico"),
             color = "red4", # Opcional: color para destacar la etiqueta de México
             size = 3) + # Ajuste de tamaño para mejor visibilidad
  labs(x = "Años de Escolaridad Promedio", 
       y = "Log del PIB per Capita (US$)", 
       title = "Relación entre Escolaridad y PIB Percapita",
       caption = "Fuente: World Developement Indicators, World Bank. \n
       Los puntos rojos representan a México en los años 2012-2018.") +
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "#CCCCCC", linetype = "dashed"),  # LC-neas de cuadrC-cula punteadas
        panel.background = element_rect(fill = "#F8F8F8"),  # Fondo del grC!fico mC!s claro
        plot.title =  element_text(color = "black", size = 12, face = "bold", hjust = 0.5),  # TC-tulo en negro y negrita
        plot.caption = element_text(color = "#666666", size = 8, face = "bold"),
        axis.title = element_text(color = "#555555", size = 10),  # Etiquetas de ejes mC!s oscuras
        axis.text = element_text(color = "#555555", size = 10)) 


# Gráfico de los años de educación promedio
# ==============================================================================
data <- readxl::read_excel(file.path("C:/Users/parra/Documents/Tesis/Bases", "años_educación_time.xlsx"))

data <- data %>%
  dplyr::select(all_of(c("Fecha","Hombre","Mujer"))) %>%
  pivot_longer(cols = -Fecha, names_to = "Género", values_to = "Años")

plot_a <- data  %>%
  ggplot(aes(x = Fecha, y = Años, color = Género)) +
  geom_line(linewidth = 1) +  # Línea para cada género
  labs(
    title = "Evolución de los Años de Educación por Tamaño de Género",
    x = "Fecha",
    y = "Años de Educación"
  ) +
  theme_minimal() +  # Tema minimalista
  theme(panel.grid.major = element_line(color = "#CCCCCC", linetype = "dashed"),  # LC-neas de cuadrC-cula punteadas
        panel.background = element_rect(fill = "#F8F8F8"),  # Fondo del grC!fico mC!s claro
        plot.title =  element_text(color = "black", size = 12, face = "bold", hjust = 0.5),  # TC-tulo en negro y negrita
        plot.caption = element_text(color = "#666666", size = 8, face = "bold"),
        axis.title = element_text(color = "#555555", size = 10),  # Etiquetas de ejes mC!s oscuras
        axis.text = element_text(color = "#555555", size = 10)) +
  geom_point(data =  data %>%
               filter(Fecha %in% as.POSIXct(as.Date(c("2005-04-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(data$Fecha), "%Y-%m-%d"))), tz = "UTC")),
             aes(x = Fecha, y = Años, color = Género, shape = Género), size = 3) +
  geom_text(data =  data %>%
              filter(Fecha %in% as.POSIXct(as.Date(c("2005-04-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(data$Fecha), "%Y-%m-%d"))), tz = "UTC")),
            aes(x = Fecha, y = Años, label = round(Años, 2), fontface = "bold"), vjust = 0.5, hjust = 0.5, color = "black", size = 4)
# ====
data <- readxl::read_excel(file.path("C:/Users/parra/Documents/Tesis/Bases", "años_educación_time.xlsx"))

data <- data %>%
  dplyr::select(all_of(c("Fecha","Rural","Urbano"))) %>%
  pivot_longer(cols = -Fecha, names_to = "Localidad", values_to = "Años")

plot_b <- data  %>%
  ggplot(aes(x = Fecha, y = Años, color = Localidad)) +
  geom_line(linewidth = 1) +  # Línea para cada género
  labs(
    title = "Evolución de los Años de Educación por Tamaño de Localidad",
    x = "Fecha",
    y = "Años de Educación",
    color = "Localidad"
  ) +
  theme_minimal() +  # Tema minimalista
  theme(panel.grid.major = element_line(color = "#CCCCCC", linetype = "dashed"),  # LC-neas de cuadrC-cula punteadas
        panel.background = element_rect(fill = "#F8F8F8"),  # Fondo del grC!fico mC!s claro
        plot.title =  element_text(color = "black", size = 12, face = "bold", hjust = 0.5),  # TC-tulo en negro y negrita
        plot.caption = element_text(color = "#666666", size = 8, face = "bold"),
        axis.title = element_text(color = "#555555", size = 10),  # Etiquetas de ejes mC!s oscuras
        axis.text = element_text(color = "#555555", size = 10)) +
  geom_point(data =  data %>%
               filter(Fecha %in% as.POSIXct(as.Date(c("2005-04-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(data$Fecha), "%Y-%m-%d"))), tz = "UTC")),
             aes(x = Fecha, y = Años, color = Localidad, shape = Localidad), size = 3) +
  geom_text(data =  data %>%
              filter(Fecha %in% as.POSIXct(as.Date(c("2005-04-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(data$Fecha), "%Y-%m-%d"))), tz = "UTC")),
            aes(x = Fecha, y = Años, label = round(Años, 2), fontface = "bold"), vjust = 0.5, hjust = 0.5, color = "black", size = 4)

# ====
data <- readxl::read_excel(file.path("C:/Users/parra/Documents/Tesis/Bases", "años_educación_time.xlsx"))

data <- data %>%
  dplyr::select(all_of(c("Fecha","informal","formal"))) %>%
  pivot_longer(cols = -Fecha, names_to = "Empleo", values_to = "Años")

plot_c <- data  %>%
  ggplot(aes(x = Fecha, y = Años, color = Empleo)) +
  geom_line(linewidth = 1) +  # Línea para cada género
  labs(
    title = "Evolución de los Años de Educación por Tipo de Empleo",
    x = "Fecha",
    y = "Años de Educación",
    color = "Empleo"
  ) +
  theme_minimal() +  # Tema minimalista
  theme(panel.grid.major = element_line(color = "#CCCCCC", linetype = "dashed"),  # LC-neas de cuadrC-cula punteadas
        panel.background = element_rect(fill = "#F8F8F8"),  # Fondo del grC!fico mC!s claro
        plot.title =  element_text(color = "black", size = 12, face = "bold", hjust = 0.5),  # TC-tulo en negro y negrita
        plot.caption = element_text(color = "#666666", size = 8, face = "bold"),
        axis.title = element_text(color = "#555555", size = 10),  # Etiquetas de ejes mC!s oscuras
        axis.text = element_text(color = "#555555", size = 10)) +
  geom_point(data =  data %>%
               filter(Fecha %in% as.POSIXct(as.Date(c("2005-04-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(data$Fecha), "%Y-%m-%d"))), tz = "UTC")),
             aes(x = Fecha, y = Años, color = Empleo, shape = Empleo), size = 3) +
  geom_text(data =  data %>%
              filter(Fecha %in% as.POSIXct(as.Date(c("2005-04-01", "2010-01-01", "2015-01-01", "2020-01-01", format(max(data$Fecha), "%Y-%m-%d"))), tz = "UTC")),
            aes(x = Fecha, y = Años, label = round(Años, 2), fontface = "bold"), vjust = 0.5, hjust = 0.5, color = "black", size = 4)


# Relación entre ingreso y nivel educativo (años de educación)
data <- readxl::read_excel(file.path("C:/Users/parra/Documents/Tesis/Bases", "entidad_ingreso_educación.xlsx"))

plot_d <- data  %>%
  filter(Fecha >= as.POSIXct(dmy("01-10-2015"))) %>%
  ggplot(aes(x = anios_esc, y = log(ing_x_hrs))) +
  geom_point(color = "darkblue", size = 2) +  # Línea para cada género
  labs(
    title = "Relación entre Escolaridad e Ingreso en México: 2015-2023",
    caption = "Fuente: ENOE",
    x = "Años de Escolaridad",
    y = "Log de Ingreso por Hora Trabajada",
  ) +
  theme_minimal() +  # Tema minimalista
  theme(panel.grid.major = element_line(color = "#CCCCCC", linetype = "dashed"),  # LC-neas de cuadrC-cula punteadas
        panel.background = element_rect(fill = "#F8F8F8"),  # Fondo del grC!fico mC!s claro
        plot.title =  element_text(color = "black", size = 12, face = "bold", hjust = 0.5),  # TC-tulo en negro y negrita
        plot.caption = element_text(color = "#666666", size = 8, face = "bold"),
        axis.title = element_text(color = "#555555", size = 10),  # Etiquetas de ejes mC!s oscuras
        axis.text = element_text(color = "#555555", size = 10))
