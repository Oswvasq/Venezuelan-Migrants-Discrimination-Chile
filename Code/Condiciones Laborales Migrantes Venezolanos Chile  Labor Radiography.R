#*******************************************************************************
# PROJECT:  Migración y divergencias en el mercado de trabajo: Analisis de las
#           condiciones laborales de los venezolanos en Chile frente a la    
#           población local, 2015-2022
#
# SCRIPT:   Labor Radiography
#
# AUTHOR:   Oswel Yosue Martinez Vasquez
#          
#
# ADVISOR:  Jesús Marcano
#
# DATE:     See file properties
#
#*******************************************************************************

#*******************************************************************************
# 1. Load Data
#*******************************************************************************

load(
  file = paste0(
    out_folder_data,
    "/Condiciones Laborales Migrantes Venezolanos Chile.RData"
  )
)


#*******************************************************************************
# 2. Labor Radiography of Chile (Tidy - Tables)
#*******************************************************************************

############### Labor Participation, Occupation and Unemployment ###############


participacion_laboral <- data_casen_expr %>%
  filter(nacionalidad_origen %in% c("Chilena","Venezolana"),
         edad >= 15, 
         !is.na(empleo)) %>% 
  group_by(nacionalidad_origen, año) %>%
  summarize(
    desempleados = survey_total(empleo == "Desocupado"),
    ocupados = survey_total(empleo == "Ocupado"),
    poblacion_activa = survey_total(empleo %in% c("Ocupado", "Desocupado")),
    poblacion_total = survey_total(empleo %in% c("Ocupado", "Desocupado", "Inactivo"))
  ) %>%
  mutate(
    tasa_desempleo = desempleados / poblacion_activa,
    tasa_ocupacion = ocupados / poblacion_activa,
    tasa_participacion = poblacion_activa / poblacion_total
  )


participacion_laboral_2 <- pivot_longer(participacion_laboral,
                                        cols = c(tasa_desempleo,
                                                 tasa_ocupacion, 
                                                 tasa_participacion), 
                                        names_to = "variable", values_to = "valor")



Figura_20_1 <- participacion_laboral_2 %>%
  filter(nacionalidad_origen != "Venezolana") %>%
  mutate(variable = case_when(
    variable == "tasa_desempleo" ~ "Tasa de Desempleo",
    variable == "tasa_ocupacion" ~ "Tasa de Ocupación",
    variable == "tasa_participacion" ~ "Tasa de Participación",
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(x = año, y = valor, color = variable)) +
  geom_line(size = 1.5) +
  facet_wrap(~ variable, scales = "free_y") +
  labs(
    x = "Año",
    y = "Tasa (%)",
    color = "Indicador"
  ) +
  scale_color_manual(values = c ("Tasa de Desempleo" = "#029daf", 
                                 "Tasa de Ocupación" = "#EC407A", 
                                 "Tasa de Participación" = "#AF7AC5"
  )) + 
  scale_x_continuous(breaks = unique(participacion_laboral_2$año)) + 
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 15),
    axis.title.x = element_text(margin = margin(t = 20), size = 15),
    legend.title = element_text(size = 17), 
    legend.text = element_text(size = 17), 
    strip.text = element_text(size = 18)
  )



Figura_20_2 <- participacion_laboral_2 %>%
  filter(nacionalidad_origen != "Chilena") %>%
  mutate(variable = case_when(
    variable == "tasa_desempleo" ~ "Tasa de Desempleo",
    variable == "tasa_ocupacion" ~ "Tasa de Ocupación",
    variable == "tasa_participacion" ~ "Tasa de Participación",
    TRUE ~ NA_character_
  )) %>%
  ggplot(aes(x = año, y = valor, color = variable)) +
  geom_line(size = 1.5) +
  facet_wrap(~variable, scales = "free_y") +
  labs(
    x = "Año",
    y = "Tasa (%)",
    color = "Indicador"
  ) +
  scale_color_manual(values = c ("Tasa de Desempleo" = "#029daf", 
                                 "Tasa de Ocupación" = "#EC407A", 
                                 "Tasa de Participación" = "#AF7AC5"
  )) + 
  scale_x_continuous(breaks = unique(participacion_laboral_2$año)) + 
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 16, color = "black"),
    axis.text.y = element_text(size = 16, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 15),
    axis.title.x = element_text(margin = margin(t = 20), size = 15),
    legend.title = element_text(size = 17), 
    legend.text = element_text(size = 17), 
    strip.text = element_text(size = 18)
  )
############################ Income Mean #############################

ingreso <- data_casen_expr %>%
  filter(nacionalidad_origen %in% c("Chilena", "Venezolana"),
         edad >= 15,
         !is.na(ingreso)) %>%
  group_by(nacionalidad_origen, año) %>%
  summarize(mediana_ingreso = survey_median(ingreso, vartype = "ci", na.rm = TRUE))


Figura_23 <- ingreso %>%
  ggplot(aes(x = año, y = mediana_ingreso, fill = nacionalidad_origen)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = formatC(mediana_ingreso, format = "d", big.mark = ".", decimal.mark = ",")),
            position = position_dodge(width = 0.9),
            vjust = ifelse(ingreso$nacionalidad_origen == "Chilena", -0.5, -0.8),
            hjust = ifelse(ingreso$nacionalidad_origen == "Chilena", 0.7, 0.2),
            size = 4) +
  labs(
    x = "Año",
    y = "Ingreso Promedio Mensual",
    fill = "Nacionalidad"
  ) +
  scale_fill_manual(values = c("Chilena" = "#029daf", "Venezolana" = "#ffc219")) +
  scale_x_continuous(breaks = unique(ingreso$año)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )



########################## Agreed weekly working hours #########################




horas_pactadas <- data_casen_expr %>%
  filter(nacionalidad_origen %in% c("Chilena", "Venezolana"),
         edad >= 15,
         !is.na(horas_pactadas)) %>%
  group_by(nacionalidad_origen, año) %>%
  summarize(horas_pactadas = survey_mean(horas_pactadas, vartype = "ci", na.rm = TRUE))



horas_pactadas <- data_casen_expr %>%
  filter(nacionalidad_origen %in% c("Chilena", "Venezolana"),
         edad >= 15,
         !is.na(horas_pactadas)) %>%
  group_by(nacionalidad_origen, año) %>%
  summarize(horas_pactadas = survey_mean(horas_pactadas, vartype = "ci", na.rm = TRUE))



Figura_26 <- horas_pactadas %>%
  ggplot(aes(x = año, y = horas_pactadas, fill = nacionalidad_origen)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = formatC(horas_pactadas, format = "d", big.mark = ".", decimal.mark = ",")),
            position = position_dodge(width = 0.9),
            vjust = ifelse(horas_pactadas$nacionalidad_origen == "Chilena", -0.5, -0.8),
            hjust = ifelse(horas_pactadas$nacionalidad_origen == "Chilena", 0.7, 0.2),
            size = 4) +
  labs(
    x = "Año",
    y = "Horas Trabajadas Semanalmente",
    fill = "Nacionalidad"
  ) +
  scale_fill_manual(values = c("Chilena" = "#029daf", "Venezolana" = "#ffc219")) +
  scale_x_continuous(breaks = unique(horas_pactadas$año)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )





######################## Effective weekly working hours ########################


horas_efectivas <- data_casen_expr %>%
  filter(nacionalidad_origen %in% c("Chilena", "Venezolana"),
         edad >= 15,
         !is.na(horas_efectivas)) %>%
  group_by(nacionalidad_origen, año) %>%
  summarize(horas_efectivas = survey_mean(horas_efectivas, vartype = "ci", na.rm = TRUE))



Figura_29 <- horas_efectivas %>%
  ggplot(aes(x = año, y = horas_efectivas, fill = nacionalidad_origen)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = formatC(horas_efectivas, format = "d", big.mark = ".", decimal.mark = ",")),
            position = position_dodge(width = 0.9),
            vjust = ifelse(horas_efectivas$nacionalidad_origen == "Chilena", -0.5, -0.8),
            hjust = ifelse(horas_efectivas$nacionalidad_origen == "Chilena", 0.7, 0.2),
            size = 4) +
  labs(
    x = "Año",
    y = "Horas Trabajadas Semanalmente",
    fill = "Nacionalidad"
  ) +
  scale_fill_manual(values = c("Chilena" = "#029daf", "Venezolana" = "#ffc219")) +
  scale_x_continuous(breaks = unique(horas_efectivas$año)) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13)
  )





#*******************************************************************************
# 4. Save Data 1 
#*******************************************************************************


write.xlsx(list(
  participacion_laboral = participacion_laboral, 
  participacion_laboral_2 = participacion_laboral_2,
  ingreso = ingreso,
  horas_pactadas = horas_pactadas, 
  horas_efectivas = horas_efectivas
), 
paste0(
  out_folder_tables,
  "/Radiografía Laboral.xlsx"
)
)


ggsave(filename = "Figura 20_1.jpg", 
       plot = Figura_20_1, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 50,  
       height = 30,  
       units = "cm")


ggsave(filename = "Figura 20_2.jpg", 
       plot = Figura_20_2, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 50,  
       height = 30,  
       units = "cm")


ggsave(filename = "Figura 23.jpg", 
       plot = Figura_23, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 30,  
       height = 30,  
       units = "cm")


ggsave(filename = "Figura 26.jpg", 
       plot = Figura_26, 
       path = paste0(
         out_folder_figures),
       dpi = 500)


ggsave(filename = "Figura 29.jpg", 
       plot = Figura_29, 
       path = paste0(
         out_folder_figures),
       dpi = 500)




rm(
  participacion_laboral, 
  participacion_laboral_2,
  ingreso, 
  horas_efectivas,
  horas_pactadas,
  Figura_20_1, 
  Figura_20_2, 
  Figura_23, 
  Figura_26,
  Figura_29
  
)

gc()


#*******************************************************************************
# 4. Save Data 2 
#*******************************************************************************

save.image(
  file = paste0(
    out_folder_data,
    "/Condiciones Laborales Migrantes Venezolanos Chile.RData"
  )
)





