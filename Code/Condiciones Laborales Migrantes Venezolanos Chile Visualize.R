#*******************************************************************************
# PROJECT:  Migración y divergencias en el mercado de trabajo: Analisis de las
#           condiciones laborales de los venezolanos en Chile frente a la    
#           población local, 2015-2022
#
# SCRIPT:   Visualize
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
# 1. Tidy Data
#*******************************************************************************

load(
  file = paste0(
    out_folder_data,
    "/Condiciones Laborales Migrantes Venezolanos Chile.RData"
  )
)


#*******************************************************************************
# 2. Visualize Data
#*******************************************************************************


# Figura 10 - Muestra Representativa de la Población de Estudio: Chilenos y
# Venezolanos Radicados en Chile. Período 2015-2022.

  
Figura_10 <-  ggplot(data_casen_or,
                     aes(x = factor(año), fill = nacionalidad_origen)) +
  geom_bar(data = subset(data_casen_or, nacionalidad_origen != "Otra"),
           position = "dodge") +
  geom_text(data = subset(data_casen_or, nacionalidad_origen != "Otra"), 
            stat = "count", aes(label = ..count..), vjust = 0, size = 5, hjust = -0.1) +
  scale_fill_manual(values = c ("Chilena" = "#029daf", 
                                "Venezolana" = "#ffc219")) + 
  scale_y_continuous(labels = scales::number_format()) + 
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


# Figura 11 - Muestra de Estudio: Chilenos y Venezolanos en Edad de Trabajar
# Radicados en Chile. Período 2015-2022


Figura_11 <-  ggplot(data_casen, aes(x = factor(año), fill = nacionalidad_origen)) +
  geom_bar(data = subset(data_casen, nacionalidad_origen != "Otra"),
           position = "dodge") +
  geom_text(data = subset(data_casen, nacionalidad_origen != "Otra"), 
            stat = "count", aes(label = ..count..), vjust = 0, size = 5, hjust = -0.1) +
  scale_fill_manual(values = c ("Chilena" = "#029daf", 
                                "Venezolana" = "#ffc219")) + 
  scale_y_continuous(labels = scales::number_format()) + 
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




#*******************************************************************************
# 3. Save Visualizations
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
  