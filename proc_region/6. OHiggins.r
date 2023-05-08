##############--------------Procesamiento Region 6 --------------###############

# eliminar notación científica

options(scipen=999)

#install.packages('censo2017')
library(censo2017)
library(dplyr)
library(ggplot2)
#install.packages('chilemapas')
library(chilemapas)
library(sjmisc)
library(tidyverse)

variables <- censo_tabla("variables")
variables_codificacion <- censo_tabla("variables_codificacion")


indigena_reg6 <- tbl(censo_conectar(), "zonas") %>% 
  mutate(
    region = substr(as.character(geocodigo), 1, 2),
    comuna = substr(as.character(geocodigo), 1, 5)
  ) %>% 
  filter(region == "06") %>% 
  select(comuna, geocodigo, zonaloc_ref_id, region) %>%
  inner_join(select(tbl(censo_conectar(), "viviendas"), zonaloc_ref_id, vivienda_ref_id), by = "zonaloc_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "hogares"), vivienda_ref_id, hogar_ref_id), by = "vivienda_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "personas"), hogar_ref_id, indigena = p16), by = "hogar_ref_id") %>%
  collect()


indigena_reg6 %>% frq (indigena)


indigena_reg6 <- indigena_reg6 %>% 
  group_by(comuna, indigena) %>%
  summarise(cuenta = n()) %>%
  group_by(comuna) %>%
  mutate(proporcion = cuenta / sum(cuenta))


mapa_sexta <- mapa_comunas %>% 
  filter(codigo_region == "06") %>% 
  #filter(codigo_comuna != "05201") %>% 
  #filter(codigo_comuna != "05104") %>% 
  left_join(indigena_reg6, by = c("codigo_comuna" = "comuna"))


colors <- c("#DCA761","#C6C16D","#8B9C94","#628CA5","#b8c5cf")

g <- ggplot() +
  geom_sf(data = mapa_sexta %>% 
            select(codigo_comuna, geometry) %>% 
            left_join(
              mapa_sexta %>% 
                filter(indigena == 1) %>% 
                select(codigo_comuna, indigena, proporcion),
              by = "codigo_comuna"
            ),
          aes(fill = proporcion, geometry = geometry),
          size = 0.1) +
  #geom_sf_label(aes(label = comuna, geometry = geometry)) +
  scale_fill_gradientn(colours = rev(colors), name = "Porcentaje") +
  labs(title = "(%) Habitantes autoidentificados como indígenas",
       subtitle = "Región de O'Higgins") +
  theme_minimal(base_size = 11)

g

ggsave("img/poblacionindigena_(6)ohiggins.png", width = 29, height = 15, units = "cm")

#---- p16.a

indigena_reg6 <- tbl(censo_conectar(), "zonas") %>% 
  mutate(
    region = substr(as.character(geocodigo), 1, 2),
    comuna = substr(as.character(geocodigo), 1, 5)
  ) %>% 
  filter(region == "06") %>% 
  select(comuna, geocodigo, zonaloc_ref_id, region) %>%
  inner_join(select(tbl(censo_conectar(), "viviendas"), zonaloc_ref_id, vivienda_ref_id), by = "zonaloc_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "hogares"), vivienda_ref_id, hogar_ref_id), by = "vivienda_ref_id") %>%
  inner_join(select(tbl(censo_conectar(), "personas"), hogar_ref_id, indigena = p16a), by = "hogar_ref_id") %>%
  collect()


indigena_reg6 %>% frq (indigena)

