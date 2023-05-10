#install.packages('censo2017')
library(censo2017)
library(dplyr)
library(ggplot2)
#install.packages('chilemapas')
library(chilemapas)
library(sjmisc)
library(tidyverse)

# eliminar notación científica

options(scipen=999)

# cargar bbdd

#censo_descargar()

# con la bbdd instalada

variables <- censo_tabla("variables")
variables_codificacion <- censo_tabla("variables_codificacion")

variables

variables %>% filter(variable == "p16")
variables %>% filter(variable == "p16a")


indigena_total <- tbl(censo_conectar(), "zonas") %>% 
  mutate(
    region = substr(as.character(geocodigo), 1, 2),
    comuna = substr(as.character(geocodigo), 1, 5)
  ) %>% 
  #filter(region == "08") %>% 
  select(comuna, geocodigo, zonaloc_ref_id, region) %>%
  inner_join(select(tbl(censo_conectar(), "viviendas"), zonaloc_ref_id, vivienda_ref_id), by = "zonaloc_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "hogares"), vivienda_ref_id, hogar_ref_id), by = "vivienda_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "personas"), hogar_ref_id, indigena = p16), by = "hogar_ref_id") %>%
  collect()

#indigena_total <- as.data.frame(indigena_total) %>% drop_na()

i#ndigena_total %>% group_by(region) %>% frq (indigena)

#indigenas_bio %>% frq (indigena)


indigena_total <- indigena_total %>% 
  group_by(comuna, indigena) %>%
  summarise(cuenta = n()) %>%
  group_by(comuna) %>%
  mutate(proporcion = cuenta / sum(cuenta))


mapa <- mapa_comunas %>% 
  filter(codigo_comuna != "05201") %>% 
  filter(codigo_comuna != "05103") %>% 
  filter(codigo_comuna != "05104") %>% 
  left_join(indigena_total, by = c("codigo_comuna" = "comuna"))


colors <- c("#DCA761","#C6C16D","#8B9C94","#628CA5","#b8c5cf")

g <- ggplot() +
  geom_sf(data = mapa %>% 
            select(codigo_comuna, geometry) %>% 
            left_join(
              mapa %>% 
                filter(indigena == 1) %>% 
                select(codigo_comuna, indigena, proporcion),
              by = "codigo_comuna"
            ),
          aes(fill = proporcion, geometry = geometry),
          size = 0.1) +
  #geom_sf_label(aes(label = comuna, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(colors), name = "Porcentaje") +
  labs(title = "% de Habitantes Indígenas",
       subtitle = "Chile continental") +
  theme_minimal(base_size = 16)

g

ggsave("img/poblacionindigena_(0)total.png", width = 32, height = 25, units = "cm")

## p.16a

indigena_t <- tbl(censo_conectar(), "zonas") %>% 
  mutate(
    region = substr(as.character(geocodigo), 1, 2),
    comuna = substr(as.character(geocodigo), 1, 5)
  ) %>% 
  #filter(region == "10") %>% 
  select(comuna, geocodigo, zonaloc_ref_id, region) %>%
  inner_join(select(tbl(censo_conectar(), "viviendas"), zonaloc_ref_id, vivienda_ref_id), by = "zonaloc_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "hogares"), vivienda_ref_id, hogar_ref_id), by = "vivienda_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "personas"), hogar_ref_id, indigena = p16a), by = "hogar_ref_id") %>%
  collect()


indigena_t %>% frq (indigena)
indigena_t %>% group_by (region) %>% frq (indigena)
