#*******************************************************************************
# PROJECT:  Migración y divergencias en el mercado de trabajo: Analisis de las
#           condiciones laborales de los venezolanos en Chile frente a la    
#           población local, 2015-2022
#
# SCRIPT:   Tidy CASEN 2022
#
# AUTHOR:   Oswel Yosue Martinez Vasquez
#          
#
# ADVISOR:  Jesús Marcano
#
# DATE:     See file properties
#
#*******************************************************************************

data_casen_2022 <- 
  tibble(
    year = 2022
  ) %>% 
  map(
    .f = ~ archive_read(
      archive = paste0(
        in_folder_casen_2022, 
        "/",
        .,
        ".zip"
      ),
    )
  ) %>%
  map_df(read_dta)

#*******************************************************************************
# 2. Tidy Data
#*******************************************************************************


data_casen_2022 <- data_casen_2022 %>% 
  select(edad,
         sexo,
         region, 
         expr, 
         o10,
         o15,
         o16,
         o18,
         o29, 
         o32,
         activ,
         y2_hrs,
         yoprcor, 
         e6a, 
         r1b,
         r1b_pais_esp_cod, 
         r2, 
         r2_pais_esp_cod,
         s13
  ) %>% 
  rename(r1bespp_cod = r1b_pais_esp_cod,  
         r2espp_cod = r2_pais_esp_cod,
         o26 = o29, 
         o23 = o16, 
         o16 = o18, 
         ingreso = yoprcor) %>%
  mutate(año = 2022,
         ############### Inicio de Transformación de Valores Missing en NA ##############
         o10 = replace(o10,o10== -88, NA), 
         e6a = replace(e6a, e6a == -88, NA),
         y2_hrs = replace(y2_hrs, y2_hrs == -88, NA),   
         ingreso = replace(ingreso, ingreso == 0, NA),
         ############### Cierre de Transformación de Valores Missing en NA ##############
         sexo = factor( 
           x = ifelse(
             sexo == 1, 
             "Hombre", 
             "Mujer"
           )
         ),
         grupo_etario = case_when(
           edad >= 15 & edad <= 29 ~ "Joven",
           edad >= 30 & edad <= 59 ~ "Adulto",
           edad >= 60 ~ "Adulto Mayor",
           TRUE ~ NA_character_
         ),
         grado_de_instruccion = case_when(
           e6a == 1 ~ "Ninguno",
           e6a %in% c(2,3,4) ~ "Parvularia",
           e6a == 5 ~ "Especial",
           e6a %in% c(6,7) ~ "Basica",
           e6a %in% c(8,9,10,11)  ~ "Media",
           e6a %in% c(12,13,14,15,16) ~ "Superior",
           e6a == 17 ~ "Postgrado", 
           TRUE ~ NA_character_
         ), 
         empleo = case_when(
           activ == 1  ~ "Ocupado",
           activ == 2  ~ "Desocupado", 
           activ == 3  ~ "Inactivo", 
           TRUE  ~ NA_character_
         ), 
         empleo_bi = ifelse( 
           empleo == "Ocupado", 
           1, 
           ifelse(
             empleo == "Desocupado", 
             0, 
             NA
           )
         ),
         nacionalidad_origen = ifelse(
           r1b == 1 |
             r1b == 2, 
           "Chilena", 
           ifelse(
             r1b == 3 & 
               r1bespp_cod== 513, 
             "Venezolana", 
             "Otra"
           )
         ),
         nacionalidad_procedencia = ifelse( 
           (r1b %in% c(1,2)) &
             (r2  %in% c(1,2)),
           "Chilena",
           ifelse(
             (r1b == 3) & (r2 == 3) & (r2espp_cod == 513),
             "Venezolana", 
             "Otra"
           )
         ),
         situacion_ocupacional = case_when( 
           o15 %in% c(3,4,5,6,7,8) ~ "Asalariado", 
           o15 == 1 ~ "Empleador", 
           o15 == 2 ~ "Cuenta Propia", 
           TRUE ~ NA_character_
         ), 
         ocupacion = case_when(
           o15 == 1 ~ "Patrón o empleador",
           o15 == 2 ~ "Trabajador por cuenta propia",
           o15 == 3 ~ "Empleado u obrero del sector público (Gobierno Central o Municipal)",
           o15 == 4 ~ "Empleado u obrero de empresas públicas",
           o15 == 5 ~ "Empleado u obrero del sector privado",
           o15 == 6 ~ "Servicio doméstico puertas adentro",
           o15 == 7 ~ "Servicio doméstico puertas afuera",
           o15 == 8 ~ "FF.AA. y del Orden",
           o15 == 9 ~ "Familiar no remunerado",
           TRUE ~ NA_character_
         ), 
         contratacion = case_when(
           o16 == 1 ~ "Asalariados con Contrato a Plazo Indefinido",
           o16 == 2 ~ "Asalariados con Contrato a Plazo Fijo",
           TRUE ~ NA_character_
         ), 
         segundo_empleo = case_when(
           o26 == 1 &             
             empleo == "Ocupado" & 
             situacion_ocupacional == "Asalariado" ~ "1", # Posee Segundo Empleo
           o26 == 2 &             
             empleo == "Ocupado" & 
             situacion_ocupacional == "Asalariado" ~ "0", # No Posee Segundo Empleo,
           TRUE ~  NA_character_
         ), 
         ocupacion_formal = case_when(
           s13 %in% c(1,2) & o32 == 1  ~ "1", # Ocupación Formal
           o15 == 9  | s13 %in% c(4) | o32 == 6  ~ "0", # Ocupación Informal
           TRUE ~ NA_character_
         ), 
         ocupados_cotizacion = case_when(
           o32 %in% c(1,2,3,4,5) ~ "1", # Cotiza en el Sistema de Pensiones
           o32 == 6 ~ "0", # No Cotiza en el Sistema de Pensiones
           TRUE ~ NA_character_
         ),
         y2_hrs = (y2_hrs / 4)
  ) %>% 
  rename(horas_efectivas = o10, 
         horas_pactadas = y2_hrs) %>% 
  select(-o23, 
         -o16,
         -s13,
         -o32)


#*******************************************************************************
# 3. Save Data
#*******************************************************************************

save.image(
  file = paste0(
    out_folder_data,
    "/Condiciones Laborales Migrantes Venezolanos Chile.RData"
  )
)


