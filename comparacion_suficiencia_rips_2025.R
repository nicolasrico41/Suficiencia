rm(list=ls())

packageList<-c("tidyverse", "stringr", "RCurl", "glue","rgeos","readr","ggrepel",
               "haven","raster","rgdal","survey", "readxl", "gridExtra","xlsx",
               "XLConnect", "scales","colorspace", "broom","janitor","rlang",
               "samplingbook", "sf", "openxlsx", "RSQLite", "DBI", "networkD3",
               "ggalluvial", "ggplot2", "skimr", "lubridate", "svglite", "httr",
               "jsonlite", "purrr","robotstxt", "XML", "ggdensity", "gganimate",
               "ggpubr", "readxlsb")

lapply(packageList,require,character.only=TRUE)

# Este script se usa para realizar cálculos y graficas de las frecuencias de los
# RIPS y suficiencia 2019 - 2023

setwd("C:/Users/Usuario/OneDrive - ADRES/Bases de datos")

rips <- readr::read_csv(
  "RIPS/Datos/csv/rips_anio_cups_eps.csv"
)

suficiencia <- readr::read_csv(
  "UPC/suficiencia_anio_eps_acups_cups.csv"
)

acups <- readr::read_csv(
  "RIPS/Datos/csv/acups_long.csv"
)

## Analisis de datos ----

suficiencia <- suficiencia %>% 
  dplyr::mutate(
    fuente = "Suficiencia"
  )

# Union suficiencia RIPS

rips_suficiencia <- rips %>% 
  bind_rows(
    suficiencia
  )

# Crear variable EPS

rips_suficiencia <- rips_suficiencia %>% 
  dplyr::mutate(
    DESC_EPS = case_when(
      STR_COD_EPS %in% c("EPS037","EPSS41", "EPSS37", "EPS041") ~ "NUEVA EPS",
       STR_COD_EPS %in% c("EPS005", "EPSS05") ~ "EPS SANITAS",
       STR_COD_EPS %in% c("EPS012", "EPSS12") ~ "COMFENALCO VALLE",
       STR_COD_EPS %in% c("EPS018", "EPSS18") ~ "SERVICIO OCCIDENTAL DE SALUD EPS SOS",
       STR_COD_EPS %in% c("EPS008", "EPSS08") ~ "COMPENSAR EPS",
       STR_COD_EPS %in% c("EPS002", "EPSS02") ~ "SALUD TOTAL EPS S.A.",
       STR_COD_EPS %in% c("EPS010", "EPSS10") ~ "EPS SURA",
       STR_COD_EPS %in% c("EPS017", "EPSS17") ~ "FAMISANAR",
      T ~ "NA"
    ),
    ACUP = case_when(
      ACUP == "Atención de parto por cesárea" ~ "Atención cesárea",
      ACUP == "Atención en Unidad de Cuidados Intensivos" ~ "Atención UCI",
      ACUP == "Cirugía del Sistema Nervioso Central" ~ "Cir.Sistema Nervioso",
      ACUP == "Cirugía General y especializada excepto alto costo" ~ "Cir. no alto costo",
      ACUP == "Consulta Médica Especializada" ~ "Consulta Especializada",
      ACUP == "Hospitalización general y especialidades básicas" ~ "Hospitalización básica",
      ACUP == "Otros Medios Dx y tratamiento especializado" ~ "Medios DX trat. esp",
      ACUP == "Otros procedimientos no quirurgicos y miscelaneos" ~ "No quirúrgicos o esp",
      ACUP == "Tratamiento de la ERC mediante diálisis" ~ "ERC diálisis",
      T ~ ACUP
    )
  )

acups_2024 <- acups %>% 
  filter(
    `AÑO` == 2023
  ) %>% 
  dplyr::mutate(
    `AÑO` = case_when(
      `AÑO` == 2023 ~ 2024,
      T ~ NA
    )
  )

acups <- acups %>% 
  bind_rows(
    acups_2024
  )

rips_suficiencia <- rips_suficiencia %>% 
  left_join(
    acups,
    by = c("cups" = "ACTIVIDAD",
           "anio" = "AÑO")
  )



rips_suficiencia <- rips_suficiencia %>% 
  dplyr::mutate(
    nt_denomina = case_when(
      nt_denomina == "Atención de parto por cesárea" ~ "Atención cesárea",
      nt_denomina == "Atención en Unidad de Cuidados Intensivos" ~ "Atención UCI",
      nt_denomina == "Cirugía del Sistema Nervioso Central" ~ "Cir.Sistema Nervioso",
      nt_denomina == "Cirugía General y especializada excepto alto costo" ~ "Cir. no alto costo",
      nt_denomina == "Consulta Médica Especializada" ~ "Consulta Especializada",
      nt_denomina == "Hospitalización general y especialidades básicas" ~ "Hospitalización básica",
      nt_denomina == "Otros Medios Dx y tratamiento especializado" ~ "Medios DX trat. esp",
      nt_denomina == "Otros procedimientos no quirurgicos y miscelaneos" ~ "No quirúrgicos o esp",
      nt_denomina == "Tratamiento de la ERC mediante diálisis" ~ "ERC diálisis",
      nt_denomina == "Unidad de cuidados intensivos neonatales" ~ "UCI neonatales",
      T ~ nt_denomina
    )
  )
## Gráfica general por A-CUPS ----

rips_suficiencia_agrupado <- rips_suficiencia %>%
  dplyr::filter(
    anio >= 2019 & anio <= 2023
  ) %>% 
  group_by(anio, ACUP, fuente) %>% 
  summarise(registros = sum(registros, na.rm = T),
            total_pacientes = sum(pacientes, na.rm = T))

graf_rips_sufi <- rips_suficiencia_agrupado %>% 
  filter(!(ACUP %in% c("null","Atención UCI","Traslado de pacientes",
                       "Actividades p y p"))
  ) %>% 
  ggplot() +
  geom_line(aes(x = anio,
                y  = registros,
                color = fuente),
            linewidth = 1.2
  ) +
  scale_color_manual(values = c("RIPS" = "#4E67B4", "Suficiencia" = "red")) +
  scale_y_continuous(labels = label_number(big.mark = ".", 
                                           decimal.mark = ",")) +
  theme_minimal() +
  labs(x = "",
       y = "",
       color = "") +
  theme(legend.title = element_text(face = "bold",
                                    size = 12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.position = "bottom") +
  facet_wrap(~ ACUP, scales = "free")

ggsave(
  "RIPS/DATOS/Graficas/tendencias_rips_nuevos_8_eps.jpg",
  graf_rips_sufi,
  width = 14,
  height = 10,
  dpi = 300
)

lista_eps <- unique(rips_suficiencia$DESC_EPS)

funcion_graf_eps <- function(x){
  
  rips_suficiencia_agrupado <- rips_suficiencia %>%
    dplyr::filter(
      anio >= 2019 & anio <= 2023,
      DESC_EPS == x
    ) %>% 
    group_by(anio, ACUP, fuente) %>% 
    summarise(registros = sum(registros, na.rm = T),
              total_pacientes = sum(pacientes, na.rm = T))
  
  
  graf_rips_sufi <- rips_suficiencia_agrupado %>% 
    filter(!(ACUP %in% c("null","Atención UCI","Traslado de pacientes",
                         "Actividades p y p"))
    ) %>% 
    ggplot() +
    geom_line(aes(x = anio,
                  y  = registros,
                  color = fuente),
              linewidth = 1.2
    ) +
    scale_color_manual(values = c("RIPS" = "#4E67B4", "Suficiencia" = "red")) +
    scale_y_continuous(labels = label_number(big.mark = ".", 
                                             decimal.mark = ",")) +
    theme_minimal() +
    labs(x = "",
         y = "",
         color = "") +
    theme(legend.title = element_text(face = "bold",
                                      size = 12),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 12),
          legend.position = "bottom") +
    facet_wrap(~ ACUP, scales = "free")
  
  ggsave(
    glue("RIPS/DATOS/Graficas/tendencias_rips_nuevos_{x}.jpg"),
    graf_rips_sufi,
    width = 14,
    height = 10,
    dpi = 300
  )
  
}

lapply(lista_eps, funcion_graf_eps)

### Ejercicio con NT's viejos -------------

## Gráfica general por A-CUPS ----

rips_suficiencia_agrupado_vi <- rips_suficiencia %>%
  dplyr::filter(
    anio >= 2019 & anio <= 2023
  ) %>% 
  group_by(anio, nt_denomina, fuente) %>% 
  summarise(registros = sum(registros, na.rm = T),
            total_pacientes = sum(pacientes, na.rm = T))

graf_rips_sufi_vi <- rips_suficiencia_agrupado_vi %>% 
  filter(!(nt_denomina %in% c("null","Atención UCI","Traslado de pacientes",
                       "Actividades p y p", "NA")),
         !is.na(nt_denomina)
  ) %>% 
  ggplot() +
  geom_line(aes(x = anio,
                y  = registros,
                color = fuente),
            linewidth = 1.2
  ) +
  scale_color_manual(values = c("RIPS" = "#4E67B4", "Suficiencia" = "red")) +
  scale_y_continuous(labels = label_number(big.mark = ".", 
                                           decimal.mark = ",")) +
  theme_minimal() +
  labs(x = "",
       y = "",
       color = "") +
  theme(legend.title = element_text(face = "bold",
                                    size = 12),
        axis.text = element_text(size = 10),
        legend.text = element_text(size = 12),
        legend.position = "bottom") +
  facet_wrap(~ nt_denomina, scales = "free")

ggsave(
  "RIPS/DATOS/Graficas/tendencias_rips_nuevos_8_eps_viejo_acup.jpg",
  graf_rips_sufi_vi,
  width = 14,
  height = 10,
  dpi = 300
)

lista_eps <- unique(rips_suficiencia$DESC_EPS)

funcion_graf_eps_vi <- function(x){
  
  rips_suficiencia_agrupado_vi <- rips_suficiencia %>%
    dplyr::filter(
      anio >= 2019 & anio <= 2023,
      DESC_EPS == x
    ) %>% 
    group_by(anio, nt_denomina, fuente) %>% 
    summarise(registros = sum(registros, na.rm = T),
              total_pacientes = sum(pacientes, na.rm = T))
  
  
  graf_rips_sufi <- rips_suficiencia_agrupado_vi %>% 
    filter(!(nt_denomina %in% c("null","Atención UCI","Traslado de pacientes",
                         "Actividades p y p")),
           !is.na(nt_denomina)
    ) %>% 
    ggplot() +
    geom_line(aes(x = anio,
                  y  = registros,
                  color = fuente),
              linewidth = 1.2
    ) +
    scale_color_manual(values = c("RIPS" = "#4E67B4", "Suficiencia" = "red")) +
    scale_y_continuous(labels = label_number(big.mark = ".", 
                                             decimal.mark = ",")) +
    theme_minimal() +
    labs(x = "",
         y = "",
         color = "") +
    theme(legend.title = element_text(face = "bold",
                                      size = 12),
          axis.text = element_text(size = 10),
          legend.text = element_text(size = 12),
          legend.position = "bottom") +
    facet_wrap(~ nt_denomina, scales = "free")
  
  ggsave(
    glue("RIPS/DATOS/Graficas/tendencias_rips_nuevos_{x}_viejos_acups.jpg"),
    graf_rips_sufi,
    width = 14,
    height = 10,
    dpi = 300
  )
  
}

lapply(lista_eps, funcion_graf_eps_vi)
