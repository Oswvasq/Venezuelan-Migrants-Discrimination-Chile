#*******************************************************************************
# PROJECT:  Migración y divergencias en el mercado de trabajo: Analisis de las
#           condiciones laborales de los venezolanos en Chile frente a la    
#           población local, 2015-2022
#
# SCRIPT:   Visualize Model 
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
# 2. 
#*******************************************************************************


################################## Employment ##################################


 Figura_21 <- emp_model %>% 
  filter(term == "nacionalidad_origenVenezolana") %>%
  mutate(model = case_when(
    model == "model_1"  ~ "Modelo 1",
    model == "model_2"  ~ "Modelo 2",
    model == "model_3"  ~ "Modelo 3",
    model == "model_4"  ~ "Modelo 4",
    model == "model_5"  ~ "Modelo 5",
    TRUE  ~ NA_character_
  ), 
  color = case_when(
    p.value < 0.05 & p.value > 0.01 ~ "p < 0.05", 
    p.value < 0.01    ~ "p < 0.01", 
    p.value > 0.05  ~ "No Significativo", 
    TRUE  ~ NA_character_
  )
  ) %>% 
  ggplot(aes(estimate, fct_rev(model), color = color)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_text(aes(label = formatC(estimate, format =
                                  "fg", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4) + 
  geom_vline(xintercept = 0, lty = 2, size = 1, color = "black") +
  labs(
    x = "Diferencia estimada entre venezolanos y chilenos",
    y = "Modelos Lineales de Probabilidad",
    color = "Significancia"
  ) + 
    scale_color_manual(values = c(
      "p < 0.05" = "#ffc219", 
      "p < 0.01" = "#5499C7",
      "No Significativo" = "#EC407A"
    )) +  
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 13),
    axis.title.x = element_text(margin = margin(t = 20), size = 13),
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12)
  )


 
 emp_model_year %>% 
   filter(term == "nacionalidad_origenVenezolana") %>%
   mutate(
   color = case_when(
     p.value < 0.05 & p.value > 0.01 ~ "p < 0.05", 
     p.value < 0.01    ~ "p < 0.01", 
     p.value > 0.05  ~ "No Significativo", 
     TRUE  ~ NA_character_
   )
   ) %>% 
   ggplot(aes(año, estimate, color =factor(color,
                                           levels = c(
                                             "p < 0.01",
                                             "p < 0.05",
                                             "No Significativo"
                                           ))))  +
   geom_point() +
   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, position = position_dodge(width = 0.75)) + 
   geom_text(aes(label = formatC(estimate, format =
                                   "fg", big.mark = ".",
                                 decimal.mark = ",")), 
             position = position_dodge(width = 0.9),
             vjust = -0.35,
             size = 4) + 
   geom_hline(yintercept = 0, lty = 2, size = 1, color = "black") +
   labs(
     y = "Diferencia estimada entre venezolanos y chilenos",
     x = "Año",
     color = "Significancia"
   ) + 
   scale_color_manual(values = c(
     "p < 0.05" = "#ffc219", 
     "p < 0.01" = "#5499C7",
     "No Significativo" = "#EC407A"
   )) +  
   scale_x_discrete(limits = c(2015,2017,2020,2022)) + 
   theme_minimal() + 
   theme(
     # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
     axis.text.x = element_text(size = 12, color = "black"),
     axis.text.y = element_text(size = 12, color = "black"),
     axis.line = element_line(color = "black"),
     # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
     #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
     panel.background = element_blank(),
     axis.title.y = element_text(margin = margin(r = 20), size = 13),
     axis.title.x = element_text(margin = margin(t = 20), size = 13),
     legend.title = element_text(size = 13), 
     legend.text = element_text(size = 12), 
     strip.text = element_text(size = 15, family = "Segoe UI Light")
   ) + facet_wrap(~model, scales = "free_x")
 


#################################### Income ####################################

 Figura_24 <- inc_model %>% 
   filter(term == "nacionalidad_origenVenezolana") %>%
   mutate(model = case_when(
     model == "model_1"  ~ "Modelo 1",
     model == "model_2"  ~ "Modelo 2",
     model == "model_3"  ~ "Modelo 3",
     model == "model_4"  ~ "Modelo 4",
     model == "model_5"  ~ "Modelo 5",
     model == "model_6"  ~ "Modelo 6",
     TRUE  ~ NA_character_
   ), 
   color = case_when(
     p.value < 0.05 & p.value > 0.01 ~ "p < 0.05", 
     p.value < 0.01  ~ "p < 0.01",
     p.value > 0.05  ~ "No Significativo", 
     TRUE  ~ NA_character_
   )
   ) %>% 
   ggplot(aes(estimate,fct_rev(model), color = factor(color,
                                                      levels = c(
                                                        "p < 0.01",
                                                        "p < 0.05",
                                                        "No Significativo"
                                                      ))))  +
   geom_point() +
   geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
   geom_text(aes(label = formatC(estimate, format =
                                   "fg", big.mark = ".",
                                 decimal.mark = ",")), 
             position = position_dodge(width = 0.9),
             vjust = -0.5,
             size = 4) + 
   geom_vline(xintercept = 0, lty = 2, size = 1, color = "black") +
   labs(
     x = "Diferencia estimada entre venezolanos y chilenos",
     y = "Modelos Semilogaritmicos Log-Lin",
     color = "Significancia"
   ) + 
   scale_color_manual(values = c(
     "p < 0.05" = "#ffc219", 
     "p < 0.01" = "#5499C7",
     "No Significativo" = "#EC407A"
   )) +  
   theme_minimal() + 
   theme(
     # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
     axis.text.x = element_text(size = 12, color = "black"),
     axis.text.y = element_text(size = 12, color = "black"),
     axis.line = element_line(color = "black"),
     # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
     #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
     panel.background = element_blank(),
     axis.title.y = element_text(margin = margin(r = 20), size = 13),
     axis.title.x = element_text(margin = margin(t = 20), size = 13),
     legend.title = element_text(size = 13), 
     legend.text = element_text(size = 12)
   )
 
 
 
 
 
 Figura_25 <- inc_model_year %>% 
   filter(term == "nacionalidad_origenVenezolana",
          model %in% c("model_1", "model_6")) %>%
   mutate(model = case_when(
     model == "model_1"  ~ "Modelo 1",
     model == "model_6"  ~ "Modelo 6",
     TRUE  ~ NA_character_
   ), 
   color = case_when(
     p.value < 0.05 & p.value > 0.01 ~ "p < 0.05", 
     p.value < 0.01  ~ "p < 0.01",
     p.value > 0.05  ~ "No Significativo", 
     TRUE  ~ NA_character_
   )
   ) %>% 
   ggplot(aes(año, estimate, color = factor(color,
                                            levels = c(
                                              "p < 0.01",
                                              "p < 0.05",
                                              "No Significativo"
                                            )))) +
   geom_point() +
   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, position = position_dodge(width = 0.75))  +
   geom_text(aes(label = formatC(estimate, format =
                                   "fg", big.mark = ".",
                                 decimal.mark = ",")), 
             position = position_dodge(width = 0.9),
             vjust = -0.5,
             size = 4) + 
   geom_hline(yintercept = 0, lty = 2, size = 1, color = "black") +
   labs(
     y = "Diferencia estimada entre venezolanos y chilenos",
     x = "Año",
     color = "Significancia"
   ) + 
   scale_color_manual(values = c(
     "p < 0.05" = "#ffc219", 
     "p < 0.01" = "#5499C7",
     "No Significativo" = "#EC407A"
   )) +  
   scale_x_discrete(limits = c(2015,2017,2020,2022)) + 
   theme_minimal() + 
   theme(
     # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
     axis.text.x = element_text(size = 12, color = "black"),
     axis.text.y = element_text(size = 12, color = "black"),
     axis.line = element_line(color = "black"),
     # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
     #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
     panel.background = element_blank(),
     axis.title.y = element_text(margin = margin(r = 20), size = 13),
     axis.title.x = element_text(margin = margin(t = 20), size = 13),
     legend.title = element_text(size = 13), 
     legend.text = element_text(size = 12), 
     strip.text = element_text(size = 15, family = "Segoe UI Light")
   ) +
   facet_wrap(~model, scales = "free_x")
 
 
 
 

########################## Agreed weekly working hours #########################


Figura_27 <- week_hour_agreed_model %>% 
  filter(term == "nacionalidad_origenVenezolana") %>%
  mutate(model = case_when(
    model == "model_1"  ~ "Modelo 1",
    model == "model_2"  ~ "Modelo 2",
    model == "model_3"  ~ "Modelo 3",
    model == "model_4"  ~ "Modelo 4",
    model == "model_5"  ~ "Modelo 5",
    model == "model_6"  ~ "Modelo 6",
    TRUE  ~ NA_character_
  ), 
  color = case_when(
    p.value < 0.05 & p.value > 0.01 ~ "p < 0.05", 
    p.value < 0.01  ~ "p < 0.01",
    p.value > 0.05  ~ "No Significativo", 
    TRUE  ~ NA_character_
  )
  ) %>% 
  ggplot(aes(estimate,fct_rev(model), color = factor(color,
                                             levels = c(
                                               "p < 0.01",
                                               "p < 0.05",
                                               "No Significativo"
                                             ))))  +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_text(aes(label = formatC(estimate, format =
                                  "fg", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4) + 
  geom_vline(xintercept = 0, lty = 2, size = 1, color = "black") +
  labs(
    x = "Diferencia estimada entre venezolanos y chilenos",
    y = "Modelos Semilogaritmicos Log-Lin",
    color = "Significancia"
  ) + 
  scale_color_manual(values = c(
    "p < 0.05" = "#ffc219", 
    "p < 0.01" = "#5499C7",
    "No Significativo" = "#EC407A"
  )) +  
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 13),
    axis.title.x = element_text(margin = margin(t = 20), size = 13),
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12)
  )





Figura_28 <- week_hour_agreed_model_year %>% 
  filter(term == "nacionalidad_origenVenezolana",
         model %in% c("model_1", "model_6")) %>%
  mutate(model = case_when(
    model == "model_1"  ~ "Modelo 1",
    model == "model_6"  ~ "Modelo 6",
    TRUE  ~ NA_character_
  ), 
  color = case_when(
    p.value < 0.05 & p.value > 0.01 ~ "p < 0.05", 
    p.value < 0.01  ~ "p < 0.01",
    p.value > 0.05  ~ "No Significativo", 
    TRUE  ~ NA_character_
  )
  ) %>% 
  ggplot(aes(año, estimate, color = factor(color,
                                           levels = c(
                                             "p < 0.01",
                                             "p < 0.05",
                                             "No Significativo"
                                           )))) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, position = position_dodge(width = 0.75))  +
  geom_text(aes(label = formatC(estimate, format =
                                  "fg", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4) + 
  geom_hline(yintercept = 0, lty = 2, size = 1, color = "black") +
  labs(
    y = "Diferencia estimada entre venezolanos y chilenos",
    x = "Año",
    color = "Significancia"
  ) + 
  scale_color_manual(values = c(
    "p < 0.05" = "#ffc219", 
    "p < 0.01" = "#5499C7",
    "No Significativo" = "#EC407A"
  )) +  
  scale_x_discrete(limits = c(2015,2017,2020,2022)) + 
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 13),
    axis.title.x = element_text(margin = margin(t = 20), size = 13),
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12), 
    strip.text = element_text(size = 15, family = "Segoe UI Light")
  ) +
  facet_wrap(~model, scales = "free_x")




######################## Effective weekly working hours ########################


Figura_30 <- week_hour_effective_model %>% 
  filter(term == "nacionalidad_origenVenezolana") %>%
  mutate(model = case_when(
    model == "model_1"  ~ "Modelo 1",
    model == "model_2"  ~ "Modelo 2",
    model == "model_3"  ~ "Modelo 3",
    model == "model_4"  ~ "Modelo 4",
    model == "model_5"  ~ "Modelo 5",
    model == "model_6"  ~ "Modelo 6",
    TRUE  ~ NA_character_
  ), 
  color = case_when(
    p.value < 0.05 & p.value > 0.01 ~ "p < 0.05", 
    p.value < 0.01  ~ "p < 0.01",
    p.value > 0.05  ~ "No Significativo", 
    TRUE  ~ NA_character_
  )
  ) %>% 
  ggplot(aes(estimate, fct_rev(model), color = factor(color,
                                             levels = c(
                                               "p < 0.01",
                                               "p < 0.05",
                                               "No Significativo"
                                             )))) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high)) +
  geom_text(aes(label = formatC(estimate, format =
                                  "fg", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4) + 
  geom_vline(xintercept = 0, lty = 2, size = 1, color = "black") +
  labs(
    x = "Diferencia estimada entre venezolanos y chilenos",
    y = "Modelos Semilogaritmicos Log-Lin",
    color = "Significancia"
  ) + 
  scale_color_manual(values = c(
    "p < 0.05" = "#ffc219", 
    "p < 0.01" = "#5499C7",
    "No Significativo" = "#EC407A"
  )) +  
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 13),
    axis.title.x = element_text(margin = margin(t = 20), size = 13),
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12)
  )





Figura_31 <-  week_hour_effective_model_year %>% 
  filter(term == "nacionalidad_origenVenezolana",
         model %in% c("model_1", "model_6")) %>%
  mutate(model = case_when(
    model == "model_1"  ~ "Modelo 1",
    model == "model_6"  ~ "Modelo 6",
    TRUE  ~ NA_character_
  ), 
  color = case_when(
    p.value < 0.05 & p.value > 0.01 ~ "p < 0.05", 
    p.value < 0.01  ~ "p < 0.01",
    p.value > 0.05  ~ "No Significativo", 
    TRUE  ~ NA_character_
  )
  ) %>% 
  ggplot(aes(año, estimate, color = factor(color,
                                           levels = c(
                                            "p < 0.01",
                                            "p < 0.05",
                                            "No Significativo"
                                           )))) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.3, position = position_dodge(width = 0.75))  +
  geom_text(aes(label = formatC(estimate, format =
                                  "fg", big.mark = ".",
                                decimal.mark = ",")), 
            position = position_dodge(width = 0.9),
            vjust = -0.5,
            size = 4) + 
  geom_hline(yintercept = 0, lty = 2, size = 1, color = "black") +
  labs(
    y = "Diferencia estimada entre venezolanos y chilenos",
    x = "Año",
    color = "Significancia"
  ) + 
  scale_color_manual(values = c(
    "p < 0.05" = "#ffc219", 
    "p < 0.01" = "#5499C7",
    "No Significativo" = "#EC407A"
  )) +  
  scale_x_discrete(limits = c(2015,2017,2022)) + 
  theme_minimal() + 
  theme(
    # axis.title = element_text(size = 12, family = "CAMPTON LIGHT"),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.text.y = element_text(size = 12, color = "black"),
    axis.line = element_line(color = "black"),
    # plot.title = element_text(size = 16, family = "Segoe UI Light", face = "bold"),
    #plot.caption = element_text(size = 11, family = "CAMPTON LIGHT"),
    panel.background = element_blank(),
    axis.title.y = element_text(margin = margin(r = 20), size = 13),
    axis.title.x = element_text(margin = margin(t = 20), size = 13),
    legend.title = element_text(size = 13), 
    legend.text = element_text(size = 12), 
    strip.text = element_text(size = 15, family = "Segoe UI Light")
  ) +
  facet_wrap(~model, scales = "free_x")






#*******************************************************************************
# 4. Save Visualizations
#*******************************************************************************


ggsave(filename = "Figura 21.jpg", 
       plot = Figura_21, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 30,  
       height = 20,  
       units = "cm")


ggsave(filename = "Figura 22.jpg", 
       plot = Figura_22, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 30,  
       height = 20,  
       units = "cm")


ggsave(filename = "Figura 24.jpg", 
       plot = Figura_24, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 30,  
       height = 20,  
       units = "cm")


ggsave(filename = "Figura 25.jpg", 
       plot = Figura_25, 
       path = paste0(
         out_folder_figures),
       dpi = 500, 
       width = 30,  
       height = 20,  
       units = "cm")


ggsave(filename = "Figura 27.jpg", 
       plot = Figura_27, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 30,  
       height = 20,  
       units = "cm")


ggsave(filename = "Figura 28.jpg", 
       plot = Figura_28, 
       path = paste0(
         out_folder_figures),
       dpi = 500, 
       width = 30,  
       height = 20,  
       units = "cm")


ggsave(filename = "Figura 30.jpg", 
       plot = Figura_30, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 30,  
       height = 20,  
       units = "cm")


ggsave(filename = "Figura 31.jpg", 
       plot = Figura_31, 
       path = paste0(
         out_folder_figures),
       dpi = 500,
       width = 30,  
       height = 20,  
       units = "cm")

rm(
  emp_model, 
  emp_model_year, 
  inc_model, 
  inc_model_year, 
  week_hour_agreed_model, 
  week_hour_agreed_model_year, 
  week_hour_effective_model, 
  week_hour_effective_model_year, 
  Figura_21, 
  Figura_22, 
  Figura_24, 
  Figura_25, 
  Figura_27, 
  Figura_28, 
  Figura_30, 
  Figura_31,

)

gc()



#*******************************************************************************
# 5. Save Data 
#*******************************************************************************

save.image(
  file = paste0(
    out_folder_data,
    "/Condiciones Laborales Migrantes Venezolanos Chile.RData"
  )
)








