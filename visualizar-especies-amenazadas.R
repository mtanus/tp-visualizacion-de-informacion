# Liberar memoria ####
rm(list = ls())

# Cargar librerías ####
library(dplyr)
library(sf)
library(tidyr)
library(ggplot2)


# Lectura de bases ####
dbEspeciesAm <- read.csv("bases/SYB66_313_202310_Threatened Species-EDITADO.csv", sep = ",")
str(dbEspeciesAm)
names(dbEspeciesAm) <- c("ISO_Country", "Country", "Year", "Series", "Value", "Footnotes", "Source" )
dbEspeciesAm <- dbEspeciesAm %>% select(ISO_Country:Value)
unique(dbEspeciesAm$Country)
unique(dbEspeciesAm$Series)
dbEspeciesAm <- dbEspeciesAm %>% 
    mutate(
        Series = case_when(
        Series ==  "Threatened Species: Vertebrates (number)" ~ "Vertebrados",
        Series ==  "Threatened Species: Invertebrates (number)" ~ "Invertebrados",
        Series ==  "Threatened Species: Plants (number)" ~ "Plantas",
        Series ==  "Threatened Species: Total (number)" ~ "Total"))

        
dbCodigoPaisesBM <- read.csv("bases/bases-anexas/codigo-paises-BM.csv", sep = ",")
str(dbCodigoPaisesBM)
unique(dbCodigoPaisesBM$Country.Name)

dbDatosPaisesBM <- read.csv("bases/bases-anexas/P_Data_Extract_From_World_Development_Indicators.csv", sep = ";")
str(dbDatosPaisesBM)
unique(dbDatosPaisesBM$Country.Name)
grep('ant', tolower(dbDatosPaisesBM$Country.Name), value = TRUE)
# Quito la , como separador decimal
grep(',', dbDatosPaisesBM$YR2020, value = TRUE)
dbDatosPaisesBM$YR2020 <- chartr(',', '.', dbDatosPaisesBM$YR2020)
dbDatosPaisesBM$YR2020 <- as.numeric(dbDatosPaisesBM$YR2020)

# sfPaises <- read_sf("bases/bases-anexas/ne_50m_admin_0_countries/ne_50m_admin_0_countries.shp")
# str(sfPaises)
# plot(st_geometry(sfPaises))


# Filtro los datasets ####
# Me quedo con los países que puedo unir por su código
dbDatosPaisesBM <- dbDatosPaisesBM %>% 
    filter(Country.Code %in% dbCodigoPaisesBM$Country.Code) %>% 
    # select(Country.Name, Country.Code, Series.Name, X2020) %>% 
    select(Country.Name, Country.Code, Series.Name, YR2020) %>% 
    mutate(indicador = case_when(
        Series.Name == "Population, total" ~ "Poblacion",
        Series.Name == "Surface area (sq. km)" ~ "Superficie_km2",
        Series.Name == "Population density (people per sq. km of land area)" ~ "Densidad_poblacional"))

unique(dbDatosPaisesBM$indicador)
dbDatosPaisesBM <- dbDatosPaisesBM %>% 
    distinct() %>% 
    select(-Series.Name) %>% 
    pivot_wider(names_from = indicador, values_from = YR2020)


# Acondiciono los datasets ####
# Uno datasets para tener el código numérico de país
dbDatosPaisesBM <- dbDatosPaisesBM %>% 
    full_join(dbCodigoPaisesBM %>% select(Country.Name, Country.Code, Region.Name, M49.Code))

# Uno datasets para tener las especies amenazadas con datos de cada país
dbEspeciesAmDatosPaises <- dbEspeciesAm %>% 
    left_join(dbDatosPaisesBM, by = c("ISO_Country" = "M49.Code"))

table(dbEspeciesAmDatosPaises$Series)
dbEspeciesAmDatosPaises <- dbEspeciesAmDatosPaises %>% 
    filter(!is.na(Poblacion)) %>% 
    filter(!grepl("Total", Series))
    

# Creo bases con datos resumen ####
# Creo una base con población y superficie de países
pobYSupPaises <- dbEspeciesAmDatosPaises %>% 
    select(Region.Name, Country, Poblacion, Superficie_km2) %>% 
    distinct()

# Creo una base con población y superficie de países
pobYSupContinentes <- pobYSupPaises %>% 
    group_by(Region.Name) %>% 
    summarise(
        Poblacion = sum(Poblacion),
        Superficie_km2 = sum(Superficie_km2))

# Creo una base con resumen mundial
table(dbEspeciesAmDatosPaises$Series)
dbEspeciesAmMundo <- dbEspeciesAmDatosPaises %>% 
    # filter(!grepl("Total", Series)) %>%
    group_by(Year, Series) %>% 
    summarise(
        Value = sum(Value)) %>% 
    mutate(
        Poblacion = sum(pobYSupPaises$Poblacion),
        Superficie_km2 = sum(pobYSupPaises$Superficie_km2))

# Creo una base con resumen según continente
dbEspeciesAmContinentes <- dbEspeciesAmDatosPaises %>% 
    # filter(!grepl("Total", Series)) %>%
    group_by(Year, Series, Region.Name) %>% 
    summarise(
        Value = sum(Value)) %>% 
    ungroup() %>% 
    full_join(pobYSupContinentes, by = c("Region.Name" = "Region.Name"))

    
# Gráficas ####
ampSuperficie <- 10^5
pobYSupPaises %>% 
    filter(grepl('argent', tolower(Country)))
pobYSupPaises %>% 
    arrange(Superficie_km2) %>% 
    filter((Superficie_km2 > (ampSuperficie * 0.95)) & (Superficie_km2 < (ampSuperficie * 1.05)))

# Especies amenazadas por superficie de continente según tiempo ####
unique(dbEspeciesAmContinentes$Series)
unique(dbEspeciesAmContinentes$Region.Name)
dbgraf_EspeciesAmContinentes <- dbEspeciesAmContinentes %>% 
    group_by(Year, Region.Name) %>% 
    summarise(
        cantidad = sum(Value)) %>% 
    full_join(pobYSupContinentes, by = c("Region.Name" = "Region.Name")) %>% 
    mutate(
        especie_km2 = ampSuperficie * cantidad / Superficie_km2)

# Quito la notación científica
options(scipen = 999)
unique(dbgraf_EspeciesAmContinentes$Year)
dbgraf_EspeciesAmContinentes %>% 
    ggplot() +
    geom_line(aes(x = Year, y = especie_km2, colour = Region.Name)) +
    geom_text(data = dbgraf_EspeciesAmContinentes %>% 
                  filter(Year == max(unique(dbgraf_EspeciesAmContinentes$Year))),
              aes(x = Year, y = especie_km2, 
                  colour = Region.Name, label = Region.Name),
              hjust = 0.25, vjust = -0.25) +
    scale_colour_manual(
        values = c("Africa" = "#6ADB88",
                   "Americas" = "#DB9352",
                   "Asia" = "#DB5DA6",
                   "Europe" = "#BD18D9",
                   "Oceania" = "#315B96")) +
    scale_x_continuous(
        limits = c(2004, 2022),
        breaks = c(2004, 2010, 2015, 2020)) +
    theme_light() +
    theme(
        legend.position = "none",
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
        # panel.background = element_blank(),
        # axis.line = element_line(colour = "black")
        ) +
    labs(title = paste0("Evolución temporal de especies amenazadas según continente"), 
         # caption = "Fuente:",
         y = paste0("Especies amenazadas / ", as.character(ampSuperficie), " km^2"),
         x = "Año"
    ) 


ggsave("../imagenes/especies-amenazadas-por-superficie-segun-continente.jpeg", dpi = 300)

# Especies amenazadas por superficie de continente (África, Asia, Europa) según tiempo ####
# unique(dbEspeciesAmContinentes$Region.Name)
# unique(dbEspeciesAmContinentes$Series)
# dbEspeciesAmContinentes %>% 
#     filter(Region.Name %in% c("Africa", "Asia", "Europe")) %>% 
#     group_by(Year, Region.Name) %>% 
#     summarise(
#         cantidad = sum(Value)) %>% 
#     left_join(pobYSupContinentes, by = c("Region.Name" = "Region.Name")) %>% 
#     mutate(
#         especie_km2 = ampSuperficie * cantidad / Superficie_km2) %>% 
#     ggplot() +
#     geom_line(aes(x = Year, y = especie_km2, colour = Region.Name)) +
#     theme_bw()
# 
# ggsave("../imagenes/especies-amenazadas-por-superficie-segun-continente-Af-As-Eu.jpeg", dpi = "print")

# Especies amenazadas por superficie de continente (América) según tiempo ####
unique(dbEspeciesAmContinentes$Region.Name)
unique(dbEspeciesAmContinentes$Series)
dbEspeciesAmContinentes %>% 
    filter(Region.Name %in% c("Americas")) %>% 
    group_by(Year, Series, Region.Name) %>% 
    summarise(
        cantidad = sum(Value)) %>% 
    left_join(pobYSupContinentes, by = c("Region.Name" = "Region.Name")) %>% 
    mutate(
        especie_km2 = ampSuperficie * cantidad / Superficie_km2) %>% 
    ggplot() +
    geom_line(aes(x = Year, y = especie_km2, colour = Series)) +
    # geom_point(aes(x = Year, y = especie_km2, colour = Series)) +
    # facet_wrap(~Series, nrow = 1) +
    scale_x_continuous(
    limits = c(2004, 2022),
    breaks = c(2004, 2010, 2015, 2020)) +
    theme_light() +
    theme(
        # legend.position = "none",
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
        # panel.background = element_blank(),
        # axis.line = element_line(colour = "black")
    ) +
    labs(title = paste0("Evolución temporal de especies amenazadas en América según taxonomía"), 
         # caption = "Fuente:",
         y = paste0("Especies amenazadas / ", as.character(ampSuperficie), " km^2"),
         x = "Año"
    ) 

ggsave("../imagenes/especies-amenazadas-por-superficie-Am-segun-especie.jpeg", dpi = "print")

unique(dbEspeciesAmContinentes$Region.Name)
unique(dbEspeciesAmContinentes$Series)
dbEspeciesAmContinentes %>%
    filter(Region.Name %in% c("Americas")) %>%
    group_by(Year, Series, Region.Name) %>%
    summarise(
        cantidad = sum(Value)) %>%
    left_join(pobYSupContinentes, by = c("Region.Name" = "Region.Name")) %>%
    mutate(
        especie_km2 = ampSuperficie * cantidad / Superficie_km2) %>%
    ggplot() +
    geom_col(aes(x = Year, y = especie_km2, fill = Series), position = position_dodge()) +
    # geom_point(aes(x = Year, y = especie_km2, colour = Region.Name)) +
    # facet_wrap(~Series, nrow = 1) +
    theme_bw()

ggsave("../imagenes/especies-amenazadas-por-superficie-Am-segun-especie-barras.jpeg", dpi = "print")

# Especies amenazadas por superficie de países (América) según tiempo ####
unique(dbEspeciesAmDatosPaises$Region.Name)
unique(dbEspeciesAmDatosPaises$Series)
dbEspeciesAmDatosPaises %>% 
    filter(Region.Name %in% c("Americas")) %>% 
    group_by(Country, Year, Series) %>% 
    summarise(
        cantidad = sum(Value)) %>% 
    left_join(pobYSupPaises, by = c("Country" = "Country")) %>% 
    mutate(
        especie_km2 = ampSuperficie * cantidad / Superficie_km2) %>% 
    ggplot() +
    geom_line(aes(x = Year, y = especie_km2, colour = Country)) +
    # geom_point(aes(x = Year, y = especie_km2, colour = Country)) +
    facet_wrap(~Series, nrow = 1) +
    theme_bw()

# Especies amenazadas por superficie de países (Argentina y limítrofes) según tiempo ####
unique(dbEspeciesAmDatosPaises$Region.Name)
unique(dbEspeciesAmDatosPaises$Series)
# dbEspeciesAmDatosPaises %>% 
#     filter(grepl("Urug", Country))
# 32 Argentina
# 68 Bolivia (Plurin. State of)
# 76  Brazil
# 152   Chile
# 600 Paraguay
# 858 Uruguay
isoPaisesLimitrofesArg <- c(32, 68, 76, 152, 600, 858)

dbEspeciesAmDatosPaises %>% 
    filter(ISO_Country %in% isoPaisesLimitrofesArg) %>% 
    select(Country) %>% 
    distinct()

dbEspeciesAmDatosPaises %>% 
    filter(ISO_Country %in% isoPaisesLimitrofesArg) %>% 
    # filter(grepl("Vertebrados", Series)) %>% 
    group_by(Country, Year, Series) %>% 
    summarise(
        cantidad = sum(Value)) %>% 
    left_join(pobYSupPaises, by = c("Country" = "Country")) %>% 
    mutate(
        especie_km2 = ampSuperficie * cantidad / Superficie_km2) %>% 
    ggplot() +
    geom_line(aes(x = Year, y = especie_km2, colour = Country)) +
    # geom_point(aes(x = Year, y = especie_km2, colour = Country)) +
    facet_wrap(~Series, nrow = 1) +
    theme_bw() +
    theme(
        legend.position = "bottom"
    ) +
    scale_colour_manual(
        values = c("Argentina" = "#BD18D9",
                   # "Bolivia (Plurin. State of)" = "#6ADB88",
                   # "Brazil" = "#DB5DA6",
                   # "Chile" = "#BD18D9",
                   # "Paraguay" = "#315B96",
                   "Bolivia (Plurin. State of)" = "#E0E0E0",
                   "Brazil" = "#D0D0D0",
                   "Chile" = "#C0C0C0",
                   "Paraguay" = "#B0B0B0",
                   "Uruguay" = "#A0A0A0")) +
    scale_x_continuous(
        limits = c(2004, 2022),
        breaks = c(2004, 2010, 2015, 2020)) +
    theme_light() +
    theme(
        legend.position = "bottom",
        # panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank()
        # panel.background = element_blank(),
        # axis.line = element_line(colour = "black")
    ) +
    labs(title = paste0("Evolución temporal de especies amenazadas en Argentina y países limítrofes según taxonomía"), 
         # caption = "Fuente:",
         y = paste0("Especies amenazadas / ", as.character(ampSuperficie), " km^2"),
         x = "Año"
    ) 


ggsave("../imagenes/especies-amenazadas-por-superficie-ArgYLim.jpeg", dpi = "print")


dbEspeciesAmDatosPaises %>% 
    filter(ISO_Country %in% isoPaisesLimitrofesArg) %>% 
    filter(grepl("Vertebrados", Series)) %>% 
    group_by(Country, Year, Series) %>% 
    summarise(
        cantidad = sum(Value)) %>% 
    left_join(pobYSupPaises, by = c("Country" = "Country")) %>% 
    mutate(
        especie_km2 = ampSuperficie * cantidad / Superficie_km2) %>% 
    ggplot() +
    geom_line(aes(x = Year, y = especie_km2, colour = Country)) +
    # geom_point(aes(x = Year, y = especie_km2, colour = Country)) +
    facet_wrap(~Series, nrow = 1) +
    theme_bw()

ggsave("../imagenes/especies-amenazadas-por-superficie-ArgYLim-vertebrados.jpeg", dpi = "print")

dbEspeciesAmDatosPaises %>% 
    filter(ISO_Country %in% isoPaisesLimitrofesArg) %>% 
    filter(grepl("Inverteb", Series)) %>% 
    group_by(Country, Year, Series) %>% 
    summarise(
        cantidad = sum(Value)) %>% 
    left_join(pobYSupPaises, by = c("Country" = "Country")) %>% 
    mutate(
        especie_km2 = ampSuperficie * cantidad / Superficie_km2) %>% 
    ggplot() +
    geom_line(aes(x = Year, y = especie_km2, colour = Country)) +
    # geom_point(aes(x = Year, y = especie_km2, colour = Country)) +
    facet_wrap(~Series, nrow = 1) +
    theme_bw()

ggsave("../imagenes/especies-amenazadas-por-superficie-ArgYLim-invertebrados.jpeg", dpi = "print")

dbEspeciesAmDatosPaises %>% 
    filter(ISO_Country %in% isoPaisesLimitrofesArg) %>% 
    filter(grepl("Plant", Series)) %>% 
    group_by(Country, Year, Series) %>% 
    summarise(
        cantidad = sum(Value)) %>% 
    left_join(pobYSupPaises, by = c("Country" = "Country")) %>% 
    mutate(
        especie_km2 = ampSuperficie * cantidad / Superficie_km2) %>% 
    ggplot() +
    geom_line(aes(x = Year, y = especie_km2, colour = Country)) +
    # geom_point(aes(x = Year, y = especie_km2, colour = Country)) +
    facet_wrap(~Series, nrow = 1) +
    theme_bw()

ggsave("../imagenes/especies-amenazadas-por-superficie-ArgYLim-plantas.jpeg", dpi = "print")


# INTENTAR GRAFICAR 2 REGIONES SEPARADAS DE LAS OTRAS 3
# INTENTAR GRAFICAR PLATO DE SPAGUETIS CON PAISES Y PROMEDIO PARA LA REGION
# INTENTAR GRAFICAR ARGENTINA Y PAISES LIMITROFES
# ELEGIR ESCALA DE COLORES
