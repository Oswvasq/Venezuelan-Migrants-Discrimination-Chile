#*******************************************************************************
# PROJECT:  Migración y divergencias en el mercado de trabajo: Analisis de las
#           condiciones laborales de los venezolanos en Chile frente a la    
#           población local, 2015-2022
#
# SCRIPT:   Visualize Methodology 
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
# 2. Chapter III Graphics - Exploratory to the Database
#*******************************************************************************




############## Representative Sample of the Study Population ###################

muestra_representativa <- data_casen_expr %>%
  group_by(nacionalidad_origen, año) %>%
  survey_count() %>%
  filter(nacionalidad_origen != "Otra")



Figura_10 <- ggplot(muestra_representativa,
                    aes(y = n, x = factor(año), fill = nacionalidad_origen))+
  geom_bar( stat = "identity", position = "dodge") +
  geom_text(aes(label = formatC(n, format =
                                  "d", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4.5) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
  scale_fill_manual(values = c ("Chilena" = "#029daf", 
                                "Venezolana" = "#ffc219")) + 
  labs(
    x = "Período",
    y = "Número de Personas",
    fill = "Nacionalidad"
  ) +
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 13)
  )


######################### Study Sample at Working Age ##########################



muestra_estudio <- data_casen_expr %>%
  filter(nacionalidad_origen != "Otra", 
         edad >= 15)  %>%
  group_by(nacionalidad_origen, año) %>%
  survey_count()


Figura_11 <- ggplot(muestra_estudio,
                    aes(y = n, x = factor(año), fill = nacionalidad_origen))+
  geom_bar( stat = "identity", position = "dodge") +
  geom_text(aes(label = formatC(n, format =
                                  "d", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4.5 ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
  scale_fill_manual(values = c ("Chilena" = "#029daf", 
                                "Venezolana" = "#ffc219")) + 
  labs(
    x = "Período",
    y = "Número de Personas",
    fill = "Nacionalidad"
  ) +
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 13)
  )





####################### Distribution by Sex in Working Age #####################


### Chilenos ###

distribucion_sexo_chilenos <- data_casen_expr %>%
  filter(nacionalidad_origen %in% "Chilena", 
         edad >= 15)  %>%
  group_by(nacionalidad_origen, año, sexo) %>%
  survey_count() 


Figura_12 <- ggplot(distribucion_sexo_chilenos,
                    aes(y = n, x = factor(año), fill = sexo))+
  geom_bar( stat = "identity", position = "dodge") +
  geom_text(aes(label = formatC(n, format =
                                  "d", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4.5 ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
  scale_fill_manual(values = c ("Hombre" = "#029daf", 
                                "Mujer" = "#ffc219")) + 
  labs(
    x = "Período",
    y = "Número de Personas",
    fill = "Género"
  ) +
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 13)
  )




### Venezolanos ###

distribucion_sexo_venezolanos <- data_casen_expr %>%
  filter(nacionalidad_origen %in% "Venezolana", 
         edad >= 15)  %>%
  group_by(nacionalidad_origen, año, sexo) %>%
  survey_count() 


Figura_13 <- ggplot(distribucion_sexo_venezolanos,
                    aes(y = n, x = factor(año), fill = sexo))+
  geom_bar( stat = "identity", position = "dodge") +
  geom_text(aes(label = formatC(n, format =
                                  "d", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4.5 ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
  scale_fill_manual(values = c ("Hombre" = "#029daf", 
                                "Mujer" = "#ffc219")) + 
  labs(
    x = "Período",
    y = "Número de Personas",
    fill = "Género"
  ) +
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 13)
  )





################### Distribution by Age Group in Working Age ###################



### Chilenos ###

distribucion_grupo_etario_chilenos <- data_casen_expr %>%
  filter(nacionalidad_origen %in% "Chilena", 
         edad >= 15)  %>%
  group_by(nacionalidad_origen, año, grupo_etario) %>%
  survey_count() 


Figura_14 <- ggplot(distribucion_grupo_etario_chilenos,
                    aes(y = n, x = factor(año), fill = factor(grupo_etario, 
                                                              levels = c(
                                                                "Joven", 
                                                                "Adulto", 
                                                                "Adulto Mayor"
                                                              ))))+
  geom_bar( stat = "identity", position = "dodge") +
  geom_text(aes(label = formatC(n, format =
                                  "d", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4.5 ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
  scale_fill_manual(values = c ("Joven" = "#029daf", 
                                "Adulto" = "#ffc219",
                                "Adulto Mayor" = "#AF7AC5"
  ), 
  name = "Grupo Etario") + 
  labs(
    x = "Período",
    y = "Número de Personas",
    fill = "Grupo Etario"
  ) +
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 13)
  )




### Venezolanos ###


distribucion_grupo_etario_venezolanos <- data_casen_expr %>%
  filter(nacionalidad_origen %in% "Venezolana", 
         edad >= 15)  %>%
  group_by(nacionalidad_origen, año, grupo_etario) %>%
  survey_count() 


Figura_15 <- 
  ggplot(distribucion_grupo_etario_venezolanos,
         aes(y = n, x = factor(año),  fill = factor(grupo_etario, 
                                                    levels = c(
                                                      "Joven", 
                                                      "Adulto", 
                                                      "Adulto Mayor"
                                                    ))))+
  geom_bar( stat = "identity", position = "dodge") +
  geom_text(aes(label = formatC(n, format =
                                  "d", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4.5 ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
  scale_fill_manual(values = c ("Joven" = "#029daf", 
                                "Adulto" = "#ffc219",
                                "Adulto Mayor" = "#AF7AC5"
  )) + 
  labs(
    x = "Período",
    y = "Número de Personas",
    fill = "Grupo Etario"
  ) +
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 13)
  )


 
############### Distribution by Educational Level and Working Age ##############



### Chilenos ###

distribucion_grado_de_instruccion_chilenos <- data_casen_expr %>%
  filter(nacionalidad_origen %in% "Chilena", 
         edad >= 15, 
         grado_de_instruccion != "NA")  %>%
  group_by(nacionalidad_origen, año, grado_de_instruccion) %>%
  survey_count() 


Figura_16 <- ggplot(distribucion_grado_de_instruccion_chilenos,
                    aes(y = n, x = factor(año), fill = factor(grado_de_instruccion, 
                                                              levels = c(
                                                                "Ninguno", 
                                                                "Parvularia",
                                                                "Basica",
                                                                "Especial",
                                                                "Media",
                                                                "Superior" 
                                                              ))))+
  geom_bar( stat = "identity", position = "dodge") +
  geom_text(aes(label = formatC(n, format =
                                  "d", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4.5 ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
  scale_fill_manual(values = c ("Ninguno" = "#AF7AC5", 
                                "Parvularia" = "#66BB6A",
                                "Basica" = "#ffc219",
                                "Especial" = "#EC407A",
                                "Media" = "#029daf",
                                "Superior" = "#5499C7"
                                
  )) + 
  labs(
    x = "Período",
    y = "Número de Personas",
    fill = "Grado de Instrucción"
  ) +
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 13)
  )


### Venezolanos ###

distribucion_grado_de_instruccion_venezolanos <- data_casen_expr %>%
  filter(nacionalidad_origen %in% "Venezolana", 
         edad >= 15)  %>%
  group_by(nacionalidad_origen, año, grado_de_instruccion) %>%
  survey_count() 


Figura_17 <- ggplot(distribucion_grado_de_instruccion_venezolanos,
                    aes(y = n, x = factor(año),  fill = factor(grado_de_instruccion, 
                                                               levels = c(
                                                                 "Ninguno", 
                                                                  "Parvularia",
                                                                  "Basica",
                                                                  "Especial",
                                                                  "Media",
                                                                  "Superior" 
                                                               ))))+
  geom_bar( stat = "identity", position = "dodge") +
  geom_text(aes(label = formatC(n, format =
                                  "d", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4.5 ) +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
  scale_fill_manual(values = c ("Ninguno" = "#AF7AC5", 
                                "Parvularia" = "#66BB6A",
                                "Basica" = "#ffc219",
                                "Especial" = "#EC407A",
                                "Media" = "#029daf",
                                "Superior" = "#5499C7"
                                
  )) + 
  labs(
    x = "Período",
    y = "Número de Personas",
    fill = "Grado de Instrucción"
  ) +
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 13)
  )




########## Distribution by occupational classification and working age #########



### Chilenos ###

distribucion_categoria_ocupacional_chilenos <- data_casen_expr %>%
  filter(nacionalidad_origen %in% "Chilena", 
         edad >= 15, 
         empleo %in% "Ocupado")  %>%
  group_by(nacionalidad_origen, año, ocupacion) %>%
  survey_count() 


Figura_18 <- ggplot(distribucion_categoria_ocupacional_chilenos,
                    aes(y = n, x = factor(año), fill =  ocupacion))+
  geom_bar( stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
  scale_fill_manual(values = c ("Empleado u obrero de empresas públicas" = "#AF7AC5", 
                                "Empleado u obrero del sector privado" = "#ffc219",
                                "Empleado u obrero del sector público (Gobierno Central o Municipal)" = "lightblue",
                                "Familiar no remunerado" = "#5499C7",
                                "FF.AA. y del Orden" = "#c78b61",
                                "Patrón o empleador" = "#EC407A", 
                                "Servicio doméstico puertas adentro" = "#f1d4af",
                                "Servicio doméstico puertas afuera" = "#66BB6A",
                                "Trabajador por cuenta propia" = "#029daf"
                                
  )) + 
  labs(
    x = "Período",
    y = "Número de Personas",
    fill = "Categoría Ocupacional"
  ) +
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 16)
  )

### Venezolanos ###

distribucion_categoria_ocupacional_venezolana <- data_casen_expr %>%
  filter(nacionalidad_origen %in% "Venezolana", 
         edad >= 15,
         empleo %in% "Ocupado")  %>%
  group_by(nacionalidad_origen, año, ocupacion) %>%
  survey_count() 


Figura_19 <- ggplot(distribucion_categoria_ocupacional_venezolana,
                    aes(y = n, x = factor(año), fill =  ocupacion))+
  geom_bar( stat = "identity", position = "dodge") +
  scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ",")) + 
  scale_fill_manual(values = c ("Empleado u obrero de empresas públicas" = "#AF7AC5", 
                                "Empleado u obrero del sector privado" = "#ffc219",
                                "Empleado u obrero del sector público (Gobierno Central o Municipal)" = "lightblue",
                                "Familiar no remunerado" = "#5499C7",
                                "FF.AA. y del Orden" = "#c78b61",
                                "Patrón o empleador" = "#EC407A", 
                                "Servicio doméstico puertas adentro" = "#f1d4af",
                                "Servicio doméstico puertas afuera" = "#66BB6A",
                                "Trabajador por cuenta propia" = "#029daf"
                                
  )) + 
  labs(
    x = "Período",
    y = "Número de Personas",
    fill = "Categoría Ocupacional"
  ) +
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 14, color = "black"),
    axis.text.y = element_text(size = 14, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 14),
    axis.title.x = element_text(margin = margin(t = 20), size = 14),
    legend.title = element_text(size = 14), 
    legend.text = element_text(size = 16)
  )




#*******************************************************************************
# 4. Save Visualizations
#*******************************************************************************

ggsave(filename = "Figura 10.jpg", 
       plot = Figura_10, 
       path = paste0(
         out_folder_figures),
       dpi = 500)



ggsave(filename = "Figura 11.jpg", 
       plot = Figura_11, 
       path = paste0(
         out_folder_figures),
       dpi = 500)


ggsave(filename = "Figura 12.jpg", 
       plot = Figura_12, 
       path = paste0(
         out_folder_figures),
       dpi = 500)


ggsave(filename = "Figura 13.jpg", 
       plot = Figura_13, 
       path = paste0(
         out_folder_figures),
       dpi = 500)


ggsave(filename = "Figura 14.jpg", 
       plot = Figura_14, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 35,  
       height = 20,  
       units = "cm")



ggsave(filename = "Figura 15.jpg", 
       plot = Figura_15, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 35,  
       height = 20,  
       units = "cm")



ggsave(filename = "Figura 16.jpg", 
       plot = Figura_16, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 40,  
       height = 20,  
       units = "cm")

ggsave(filename = "Figura 17.jpg", 
       plot = Figura_17, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 40,  
       height = 20,  
       units = "cm")

ggsave(filename = "Figura 18.jpg", 
       plot = Figura_18, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 50,  
       height = 30,  
       units = "cm")

ggsave(filename = "Figura 19.jpg", 
       plot = Figura_19, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 50,  
       height = 30,  
       units = "cm")


write.xlsx(list(
  categoria_ocupacional_cl = distribucion_categoria_ocupacional_chilenos, 
  categoria_ocupacional_ve = distribucion_categoria_ocupacional_venezolana
), 
paste0(
  out_folder_tables,
  "/methodology.xlsx"
)
)


rm(
  distribucion_grado_de_instruccion_chilenos, 
  distribucion_grado_de_instruccion_venezolanos, 
  distribucion_grupo_etario_chilenos, 
  distribucion_grupo_etario_venezolanos, 
  distribucion_sexo_chilenos, 
  distribucion_sexo_venezolanos, 
  muestra_representativa, 
  muestra_estudio, 
  distribucion_categoria_ocupacional_chilenos, 
  distribucion_categoria_ocupacional_venezolana,
  Figura_10, 
  Figura_11, 
  Figura_12, 
  Figura_13, 
  Figura_14, 
  Figura_15, 
  Figura_16, 
  Figura_17,
  Figura_18, 
  Figura_19
)

gc()



