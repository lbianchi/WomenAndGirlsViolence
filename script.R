require(tidyverse) #data wangling
require(lubridate) #working with date
require(ggplot2) # nice charts
require(janitor) # clean data
require(rnaturalearth) #countries shape files
require(rnaturalearthdata) #countries info
require(ggspatial) #add north arrow and scale bar
require(sf)

data = read.csv("violence_data.csv") %>% 
  mutate(
    year = year(dmy(Survey.Year))
  ) %>% 
  clean_names() %>% 
  dplyr::select(-c(survey_year, record_id)) %>% 
  replace_na(list(value = 0)) %>% 
  mutate(
    question = str_to_sentence(gsub("\\... ", "", question)),
    question = gsub("If she ", "If she\n", question),
    gender = ifelse(gender == "F", "Female", "Male"),
    gender = factor(gender, levels = c("Female", "Male"))
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



