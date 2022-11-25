require(tidyverse) #data wangling
require(lubridate) #working with date
require(ggplot2) # nice charts
require(janitor) # clean data
require(rnaturalearth) #countries shape files
require(rnaturalearthdata) #countries info
require(ggspatial) #add north arrow and scale bar
require(sf)
require(rstanarm)
require(bayesplot)
require(sjPlot)

data = read.csv("violence_data.csv") %>% 
  mutate(
    year = year(dmy(Survey.Year))
  ) %>% 
  clean_names() %>% 
  dplyr::select(-c(survey_year, record_id)) %>% 
  mutate(
    question = str_to_sentence(gsub("\\... ", "", question)),
    question = gsub("If she ", "If she\n", question),
    gender = ifelse(gender == "F", "Female", "Male"),
    gender = factor(gender, levels = c("Female", "Male"))
  )

generate_maps = FALSE
if(generate_maps == TRUE){
  world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
    mutate(
      brk_name = case_when(
        brk_name == "Democratic Republic of the Congo" ~ "Congo Democratic Republic",
        brk_name == "Republic of Congo" ~ "Congo",
        brk_name == "Côte d'Ivoire" ~ "Cote d'Ivoire",
        brk_name == "Dominican Rep." ~ "Dominican Republic",
        brk_name == "Kyrgyzstan" ~ "Kyrgyz Republic",
        brk_name == "Swaziland" ~ "Eswatini",
        TRUE ~ brk_name
      )
    ) %>% 
    # filter(brk_name %in% unique(data$country)) %>% 
    rename(country = brk_name) %>% 
    dplyr::select(country, continent, economy, income_grp)
  
  data_age <- data %>% 
    filter(demographics_question == "Age")
  
  data_expanded_grid <- expand.grid(
    country = unique(world$country),
    gender =  unique(data$gender),
    year = unique(data$year),
    question = unique(data$question)
  ) %>% 
    mutate(
      id = paste0(country, "_", gender, "_", question, "_", year)
    )
  
  data_expanded_grid <- data_expanded_grid %>% 
    left_join(world, by = "country")
  
  for(i in unique(data_age$demographics_response)){
    
    data_temp <- data_age %>% 
      filter(demographics_response == i) %>% 
      group_by(country, gender, question, year) %>% 
      mutate(
        id = paste0(country, "_", gender, "_", question, "_", year),
        value = sum(value, na.rm = T)
      ) %>% 
      distinct(id, .keep_all = T) %>% 
      ungroup() %>% 
      arrange(country, gender, question, year) %>% 
      dplyr::select(-c(demographics_question, demographics_response))
    
    data_temp2 <- data_expanded_grid %>% 
      left_join(data_temp %>% 
                  dplyr::select(-c(gender, year, question, country)), by = "id") %>% 
      relocate(geometry, .after = "year")  %>% 
      replace_na(list(value = 0)) %>%
      st_as_sf()
    
    for(j in sort(unique(data_temp2$year))){
      
      data_temp3 <- data_temp2 %>% 
        filter(year == j)
      
      map_result <- ggplot() +
        geom_sf(data = world, colour = "black", size = 0.5, show.legend = F) +
        geom_sf(data = data_temp3, aes(fill = value)) +
        theme_bw() + 
        theme(plot.title = element_text(size = 40, face = "bold"),
              text = element_text(size = 30),
              axis.title = element_text(face="bold"),
              axis.text.x = element_text(size = 20),
              axis.text.y = element_text(size = 20),
              panel.grid.minor = element_blank(),
              legend.title = element_text(face = "bold", size = 30),
              legend.text = element_text(size = 30),
              legend.justification = "right",
              panel.background = element_rect(fill = "aliceblue"),
              strip.text.x = element_text(face = "bold", size = 26),
              strip.background = element_rect(
                color="black", fill="#f0f0f0", linewidth=1.5, linetype="solid"
              )
        ) + 
        labs(
          title = paste0("Sociodemographic Questions by Sex, Age: ",i, " (", j,")"),
          x = NULL, y = NULL
        ) +
        scale_fill_gradient2(
          low = "white", mid = "yellow",
          high = "red", space ="Lab" ,
          midpoint = 50, 
          limits = c(0, 100),
          breaks = seq(0, 100, 10)
        ) +
        guides(fill = guide_colourbar(title = "% Agreement", barwidth = 2, barheight = 40)) +
        facet_wrap(~question*gender, ncol = 2,
                   labeller = label_wrap_gen(width=120))
      
      ggsave(filename = paste0("images/map_sexage",i,"_",j,".png"),
             plot =  map_result, dpi = 300,
             height = 80, width = 62, units = "cm")
    }
    
  }
}

if(generate_maps == TRUE){
  data_descriptive <- data %>% 
    dplyr::select(-year) %>% 
    group_by(gender, demographics_question, demographics_response, question) %>% 
    summarise(
      mean = mean(value, na.rm = T),
      sd = sd(value, na.rm = T),
      min = min(value, na.rm = T),
      max = max(value, na.rm = T),
      median = median(value, na.rm = T)
    )
  
  for(i in unique(data_descriptive$demographics_question)){
    # i = "Age"
    data_temp <- data_descriptive %>% 
      filter(demographics_question == i)
    
    g_temp <- data_temp %>% 
      ggplot(aes(x = demographics_response, y = mean, fill = gender)) +
      geom_col(position = "dodge") +
      theme_bw() + 
      theme(plot.title = element_text(size = 20, face = "bold"),
            text = element_text(size = 12),
            axis.title = element_text(face="bold"),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            panel.grid.minor = element_blank(),
            legend.title = element_text(face = "bold", size = 12),
            legend.text = element_text(size = 12),
            legend.justification = "right",
            panel.background = element_rect(fill = "aliceblue"),
            strip.text.x = element_text(face = "bold", size = 12),
            strip.background = element_rect(
              color="black", fill="#f0f0f0", linewidth=1.5, linetype="solid"
            )
      ) +
      labs(title = str_to_title(i), x = "", y = "Mean") +
      facet_wrap(~question, ncol = 2)
    
    print(g_temp)
    
  }
}

# Maps
if(generate_maps == TRUE){
  data_map <- data %>% 
    group_by(country, gender, question) %>% 
    summarise(
      mean = mean(value, na.rm = T),
      sd = sd(value, na.rm = T),
      min = min(value, na.rm = T),
      max = max(value, na.rm = T),
      median = median(value, na.rm = T)
    ) %>% 
    ungroup() %>% 
    mutate(
      id = paste0(country, "_", gender, "_", question)
    ) 
  
  world <- ne_countries(scale = "medium", returnclass = "sf") %>% 
    mutate(
      brk_name = case_when(
        brk_name == "Democratic Republic of the Congo" ~ "Congo Democratic Republic",
        brk_name == "Republic of Congo" ~ "Congo",
        brk_name == "Côte d'Ivoire" ~ "Cote d'Ivoire",
        brk_name == "Dominican Rep." ~ "Dominican Republic",
        brk_name == "Kyrgyzstan" ~ "Kyrgyz Republic",
        brk_name == "Swaziland" ~ "Eswatini",
        TRUE ~ brk_name
      )
    ) %>% 
    # filter(brk_name %in% unique(data$country)) %>% 
    rename(country = brk_name) %>% 
    dplyr::select(country)
  
  data_expanded_grid <- expand.grid(
    country = unique(world$country),
    gender =  unique(data_map$gender),
    question = unique(data_map$question)
  ) %>% 
    mutate(
      id = paste0(country, "_", gender, "_", question)
    )
  
  data_expanded_grid <- data_expanded_grid %>% 
    left_join(world, by = "country")
  
  data_temp2 <- data_expanded_grid %>% 
    left_join(data_map %>% 
                dplyr::select(-c(gender,question, country)), by = "id") %>% 
    relocate(geometry, .after = "median")  %>% 
    replace_na(list(
      mean = 0,
      sd = 0,
      min = 0,
      max = 0,
      median = 0)) %>%
    st_as_sf()
  
  map_result <- ggplot() +
    geom_sf(data = world, colour = "black", size = 0.5, show.legend = F) +
    geom_sf(data = data_temp2, aes(fill = median)) +
    theme_bw() + 
    theme(plot.title = element_text(size = 40, face = "bold"),
          text = element_text(size = 30),
          axis.title = element_text(face="bold"),
          axis.text.x = element_text(size = 20),
          axis.text.y = element_text(size = 20),
          panel.grid.minor = element_blank(),
          legend.title = element_text(face = "bold", size = 30),
          legend.text = element_text(size = 30),
          legend.justification = "right",
          panel.background = element_rect(fill = "aliceblue"),
          strip.text.x = element_text(face = "bold", size = 26),
          strip.background = element_rect(
            color="black", fill="#f0f0f0", linewidth=1.5, linetype="solid"
          )
    ) + 
    labs(
      title = paste0("Sociodemographic Questions by Sex"),
      x = NULL, y = NULL
    ) +
    scale_fill_gradient2(
      low = "white", mid = "yellow",
      high = "red", space ="Lab" ,
      midpoint = 50, 
      limits = c(0, 100),
      breaks = seq(0, 100, 10)
    ) +
    guides(fill = guide_colourbar(title = "% Agreement", barwidth = 2, barheight = 40)) +
    facet_wrap(~question*gender, ncol = 4,
               labeller = label_wrap_gen(width=120))
  
  ggsave(filename = paste0("images/map_sex.png"),
         plot =  map_result, dpi = 300,
         height = 80, width = 80, units = "cm")
}


data_model <- data %>% 
  group_by(gender, demographics_question, demographics_response, question) %>% 
  summarise(
    median = median(value, na.rm = T)/100
  ) %>% 
  ungroup()


theme_set(bayesplot::theme_default())

fit1 <- stan_betareg(median ~ question, data = data_model, link = "logit", link.phi = "log",
                     cores = 2, seed = 12345)

fit2 <- stan_betareg(median ~ question | demographics_response, data = data_model, link = "logit", link.phi = "log",
                     cores = 2, seed = 12345)


bayesplot_grid(
  pp_check(fit1), pp_check(fit2),
  xlim = c(0,1),  
  ylim = c(0,4), 
  titles = c("Model #1: Median ~ question", "Model #2: Median ~ question | demographics response"),
  grid_args = list(ncol = 2)
)

loo1 <- loo(fit1)
loo2 <- loo(fit2)
loo_compare(loo1, loo2)

# Evaluating the expected log predictive distribution
# using loo reveals that the second of the two models
# is preferred.

prior_summary(fit2)


tab_model(fit2, string.pred = "Coeffcient")

