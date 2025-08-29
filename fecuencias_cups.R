rm(list=ls())

packageList<-c("tidyverse", "stringr", "RCurl", "glue","rgeos","readr","ggrepel",
               "haven","raster","rgdal","survey", "readxl", "gridExtra","xlsx",
               "XLConnect", "scales","colorspace", "broom","janitor","rlang",
               "samplingbook", "sf", "openxlsx", "RSQLite", "DBI", "networkD3",
               "ggalluvial", "ggplot2", "skimr", "lubridate", "svglite", "httr",
               "jsonlite", "purrr","robotstxt", "XML", "ggdensity", "gganimate",
               "ggpubr", "forecast", "tsibble", "timetk", "broom", "zoo",
               "synthdid", "glue", "pracma", "BMisc", "knitr")

lapply(packageList,require,character.only=TRUE)

ruta <- "C:/Users/Usuario/OneDrive - ADRES/Bases de datos/UPC"

## Carga de datos ----

suficiencia <- read.csv(
  glue("{ruta}//Consultas_suficiencia___mesas_2025.csv")
)

nombres_cups <- read.csv(
  glue("{ruta}/cups_nombres.csv")
)

compensados <- read.csv(
  glue("{ruta}/afiliados_compensados.csv")
)

apodos_cups <- readxl::read_xlsx(
  glue("{ruta}/apodos_cups.xlsx")
)

apodos_cups <- apodos_cups %>% 
  dplyr::mutate(
    cups = str_pad(cups,6,side = "left", pad = 0)
  )

nombres_cups <- nombres_cups %>% 
  dplyr::mutate(
    cups = str_pad(cups,6,side = "left", pad = 0)
  )

suficiencia <- suficiencia %>% 
  dplyr::filter(
    !str_detect(Procedimiento, "-")
  )

## Procesamiento ----
## Se puede ser con los que no crucen por ejemllo de A-CUPS

suficiencia <- suficiencia %>% 
  dplyr::group_by(
    anio_servicio,
    Procedimiento
  ) %>% 
  summarise(
    Registros = sum(Registros, na.rm = T),
    Valor_total = sum(Valor_total, na.rm = T),
    Personas = sum(Personas, na.rm = T)
  ) %>% 
  left_join(
    compensados,
    by = c("anio_servicio" = "anio")
  ) %>% 
  dplyr::mutate(
    severidad = Valor_total/Registros,
    frecuencia = Registros/afiliados_compensados
  ) %>% 
  dplyr::filter(
    !Valor_total < 1000
  )

suficiencia_var <- suficiencia %>% 
  arrange(Procedimiento, anio_servicio) %>% 
  dplyr::group_by(Procedimiento) %>% 
  dplyr::mutate(
    var_severidad = ((severidad - lag(severidad))/ lag(severidad)) * 100,
    var_frecuencia = ((frecuencia - lag(frecuencia)) / lag(frecuencia)) * 100
  ) %>% 
  ungroup()

suficiencia_var <- suficiencia_var %>% 
  dplyr::select(
    anio_servicio,
    Procedimiento,
    var_severidad,
    var_frecuencia
  ) %>% 
  filter(
    !is.na(var_severidad),
    is.finite(var_severidad),
    is.finite(var_frecuencia)
  )

top5masmenos <- suficiencia_var %>% 
  pivot_longer(
    cols = c(var_severidad, var_frecuencia),
    names_to = "variables",
    values_to = "variacion"
  ) %>% 
  dplyr::group_by(
    anio_servicio,
    variables
  ) %>% 
  slice_max(
    order_by = variacion,
    n = 5, 
    with_ties = FALSE
  ) %>% 
  dplyr::mutate(
    tipo = "maximo"
  ) %>% 
  bind_rows(
    suficiencia_var %>% 
      pivot_longer(
        cols = c(var_severidad, var_frecuencia),
        names_to = "variables",
        values_to = "variacion"
      ) %>% 
      dplyr::group_by(
        anio_servicio,
        variables
      ) %>% 
      slice_min(
        order_by = variacion,
        n = 5, 
        with_ties = FALSE
      ) %>% 
      dplyr::mutate(
        tipo = "minimo"
      )
  ) %>% 
  ungroup()

top5masmenos <- top5masmenos %>% 
  left_join(
    nombres_cups,
    by = c("Procedimiento" = "cups")
  ) %>% 
  left_join(
    apodos_cups,
    by = c("Procedimiento" = "cups")
  )

top5masmenos$apodo <- gsub("\\\\n", "\n", top5masmenos$apodo)

top5masmenos <- top5masmenos %>% 
  dplyr::mutate(
    apodo = case_when(
      Procedimiento == "S48100" ~ "C - S48100",
      Procedimiento == "898230" ~ "C - 898230",
      T ~ apodo
    )
  )

# Severidad

top_clean_sev <- top5masmenos %>%
  filter(variables == "var_severidad") %>%
  mutate(
    variacion = as.numeric(variacion),
    tipo = as.character(tipo)
  ) %>%
  filter(!is.na(variacion), is.finite(variacion))

top_max_sev <- top_clean_sev %>%
  group_by(anio_servicio) %>%
  slice_max(order_by = variacion, n = 5, with_ties = FALSE)

top_min_sev <- top_clean_sev %>%
  group_by(anio_servicio) %>%
  slice_min(order_by = variacion, n = 5, with_ties = FALSE)


top10_por_anio_sev <- bind_rows(top_max_sev, top_min_sev) %>%
  group_by(anio_servicio) %>%
  arrange(desc(variacion)) %>%                
  mutate(
    Procedimiento_f = factor(Procedimiento, levels = unique(Procedimiento))
  ) %>%
  ungroup()

# Frecuencia

top_clean_feq <- top5masmenos %>%
  filter(variables == "var_frecuencia") %>%
  mutate(
    variacion = as.numeric(variacion),
    tipo = as.character(tipo)
  ) %>%
  filter(!is.na(variacion), is.finite(variacion))

top_max_feq <- top_clean_feq %>%
  group_by(anio_servicio) %>%
  slice_max(order_by = variacion, n = 5, with_ties = FALSE)

top_min_feq <- top_clean_feq %>%
  group_by(anio_servicio) %>%
  slice_min(order_by = variacion, n = 5, with_ties = FALSE)


top10_por_anio_feq <- bind_rows(top_max_feq, top_min_feq) %>%
  group_by(anio_servicio) %>%
  arrange(desc(variacion)) %>%                
  mutate(
    Procedimiento_f = factor(Procedimiento, levels = unique(Procedimiento))
  ) %>%
  ungroup()

estadictica_desc <- top5masmenos %>% 
  dplyr::group_by(
    # anio_servicio,
    variables,
    tipo
  ) %>% 
  summarise(
    desviacion = sd(variacion, na.rm = T),
    promedio = mean(variacion, na.rm = T),
    mediana = median(variacion, na.rm = T)
  ) %>% 
  dplyr::filter(
    tipo == "maximo"
  ) %>% 
  arrange(variables)

top_max_feq %>% 
  dplyr::group_by(
    anio_servicio
  ) %>% 
  summarise(
    maximo = max(variacion, na.rm = T)
  )

## Gráfico facet por cada variable -----

# Severidad

Paleta_colores_ADRES <- c("#00B4B3", "#00E0E0", "#00CCCC", "#00A3A3", "#008F8F",
                          "#02787A", "#022123", "#01C4C7", "#012224", "#000809",
                          "#000203", "#01292B", "#009EA0", "#003133", "#003B3D",
                          "#016B6D", "#001112", "#00292B", "#00B1B3", "#00DADC",
                          "#001819", "#007375", "#016061", "#00E6E8", "#01B3B6",
                          "#004D4F")

graf_severidad <- top10_por_anio_sev %>% 
  filter(
    tipo == "maximo"
  ) %>% 
  ggplot(
    aes(
      x = reorder(apodo,variacion),
      y = variacion
    )
  ) +
  geom_bar(
    stat = "identity",
    fill = "#004D4F"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      accuracy = 1,
      big.mark = ".",
      decimal.mark = ",",
      scale = 1/1000,
      suffix = "K %"
    )
  ) +
  coord_flip() +
  facet_wrap(
    ~ anio_servicio,
    scales = "free"
  ) + 
  theme_minimal() +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme(strip.text = element_text(face = "bold",
                                  size = 14),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold",
                                  size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10)) 

ggsave(
  glue("{ruta}//severidad_positiva_top5.jpg"),
  graf_severidad,
  width = 14,
  height = 10,
  dpi = 300
)

# Frecuencia positiva

graf_feq_positiva <- top10_por_anio_feq %>% 
  filter(
    tipo == "maximo"
  ) %>% 
  ggplot(
    aes(
      x = reorder(apodo,variacion),
      y = variacion
    )
  ) +
  geom_bar(
    stat = "identity",
    fill = "#004D4F"
  ) +
  scale_y_continuous(
    labels = scales::label_number(
      accuracy = 1,
      big.mark = ".",
      decimal.mark = ",",
      scale = 1/1000,
      suffix = "M %"
    )
  ) +
  coord_flip() +
  facet_wrap(
    ~ anio_servicio,
    scales = "free"
  ) + 
  theme_minimal() +
  labs(
    title = "",
    x = "",
    y = ""
  ) +
  theme(strip.text = element_text(face = "bold",
                                  size = 14),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold",
                                  size = 14),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 10)) 

ggsave(
  glue("{ruta}//frecuencia_positiva_top5.jpg"),
  graf_feq_positiva,
  width = 14,
  height = 10,
  dpi = 300
)

## Severidad negativa

graf_severidad_negativa <- top10_por_anio_sev %>% 
  filter(
    tipo == "minimo"
  ) %>% 
  ggplot(
    aes(
      x = reorder(apodo,variacion),
      y = variacion
    )
  ) +
  geom_bar(
    stat = "identity",
    fill = "#004D4F"
  ) +
  scale_y_continuous(
    # limits = c(-100, -95),
    # labels = function(x) {
    #   paste0("-", scales::label_number(
    #     accuracy = 1,
    #     big.mark = ".",
    #     decimal.mark = ",",
    #     suffix = "%"
    #   )(abs(x)))
    # }
    labels = scales::label_number(
      accuracy = 0.1,
      big.mark = ".",
      decimal.mark = ",",
      suffix = "%"
    )
  )+
  coord_flip() +
  facet_wrap(
    ~ anio_servicio,
    scales = "free"
  ) + 
  theme_minimal() +
  labs(
    title = "Top 5 variación % negativa de severidad por año",
    x = "",
    y = ""
  ) +
  theme(strip.text = element_text(face = "bold"),
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0.5, 
                                  face = "bold",
                                  size = 14)) 

ggsave(
  glue("{ruta}//severidad_negativa_top5.jpg"),
  graf_severidad_negativa,
  width = 14,
  height = 10,
  dpi = 300
)