library(survey)
library(srvyr)
library(sf)
library(geobr)

pns_df_weighted <- get_pns(year=2019, vars=c( "V001", "V0026", "C004","C006","C008",
                                              "C009","C01001", "C01801", 
                                              "C011","C012", "C014","D00201","D00202", "E001", 
                                              "E002", "E003", "E004", "E005",
                                              "E011","E017", "E01401", "E014011", "E01402",
                                              "E01403", "E01201", "E01403", "J007", 
                                              "J00101", "VDD004A", 
                                              "VDE001", "VDF004", "VDE014", "VDE002", "S066", "J060",
                                              "Q092", "F007011", "VDE014"), design=T)

#Transform into tbl_svy
pns_weighted <- as_survey_design(pns_df_weighted)

#Filter women who are head of the household or partner 
women_children_w <- pns_weighted %>%
  filter(C004 == "Cônjuge ou companheiro(a) de sexo diferente" |
           C004 == "Pessoa responsável pelo domicílio" |
           C004 == "Cônjuge ou companheiro(a) do mesmo sexo") %>%
  filter(C006 == "Mulher")

#select only the household with children younger than 18
women_with_children_w <- women_children_w %>%
  semi_join(ID_families_children, by = "ID_DOMICILIO")

#Variables 

women_with_children_w <- women_with_children_w %>%
  mutate(Race = factor(case_when(
    C009 == "Branca" ~ "White",
    C009 == "Preta" | C009 == "Parda" | C009 == "Amarela" | C009 == "Indígena" | C009 == "Ignorado" ~ "Non-white",
    TRUE ~ NA_character_  # Default case for handling other unexpected values
  ), levels = c("White", "Non-white")))

women_with_children_w <- women_with_children_w %>%
  mutate(Settlement = factor(case_when(
    V0026 == "Urbano" ~ "Urban",
    V0026 == "Rural"  ~ "Rural",
    TRUE ~ NA_character_  # Default case to handle other unexpected values
  ), levels = c("Urban", "Rural")))


#self reported health (collapsed)
women_with_children_w <- women_with_children_w %>% 
  mutate(SRH_binary = case_when(
    J00101 == "Muito bom" | J00101 == "Bom" ~ 1,
    J00101 == "Regular" | J00101 == "Ruim" |  J00101 == "Muito ruim" ~ 0,
    TRUE ~ NA_integer_ # Use NA_integer_ instead of NA_character_ for integer output
  ))

women_with_children_w <- women_with_children_w %>% 
  mutate(SRH_factor = factor(case_when(
    J00101 == "Muito bom" | J00101 == "Bom" ~ "Good",
    J00101 == "Regular" | J00101 == "Ruim" |  J00101 == "Muito ruim" ~ "Bad",
    TRUE ~ NA_character_ # Use NA_integer_ instead of NA_character_ for integer output
  ), levels= c( "Good", "Bad")))

women_with_children_w <- women_with_children_w %>%
  mutate(Region = factor(case_when(
    V0001 %in% c("Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo") ~ "Southeast",
    V0001 %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina") ~ "South",
    V0001 %in% c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins") ~ "North",
    V0001 %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe") ~ "Northeast",
    V0001 %in% c("Goiás", "Mato Grosso", "Mato Grosso do Sul", "Distrito Federal") ~ "Mid-West",
    TRUE ~ NA_character_
  ), levels = c("North", "Northeast", "Southeast", "South", "Mid-West")))

#employment (employed, unemployed, not employed)

women_with_children_w <- women_with_children_w %>%
  mutate(emprego = case_when(
    !is.na(VDE002) ~ VDE002,
    is.na(VDE002) ~ VDE001,  
    TRUE ~ NA_character_     
  ))

#employment dummy 
women_with_children_w <- women_with_children_w %>%
  mutate(Employment = factor(case_when(
    emprego == "Pessoas Ocupadas" ~ "Employed",
    emprego == "Pessoas desocupadas" | emprego == "Pessoas fora da força de trabalho" ~ "Unemployed/Not Employed",
    TRUE ~ NA_character_  # Correct to use NA_character_ for character output
  ), levels = c("Employed", "Unemployed/Not Employed")))

women_with_children_w <- women_with_children_w %>%
  mutate(Partnership_status = factor(ifelse(C01001 == "Não", "Lone", "Partnered"),
                                     levels = c("Lone", "Partnered")))

women_with_children_w <- women_with_children_w %>%
  rename(Age = C008)

women_with_children_w <- women_with_children_w %>%
  mutate(Age_Mother = factor(case_when(
    Age < 30 ~ "Less than 30",
    Age >= 30 & Age <= 39 ~ "30 to 39",
    Age >= 40 ~ "40 or older",
    TRUE ~ NA_character_
  ), levels = c("Less than 30", "30 to 39", "40 or older")))


women_with_children_w <- women_with_children_w %>% 
  mutate(Education = factor(case_when(
    VDD004A == "Fundamental incompleto ou equivalente" | VDD004A == "Sem instrução" ~ "No Education",
    VDD004A == "Médio incompleto ou equivalente" | VDD004A == "Fundamental completo ou equivalente" ~ "Primary",
    VDD004A == "Superior incompleto ou equivalente" | VDD004A == "Médio completo ou equivalente" ~ "Secondary",
    VDD004A == "Superior completo" ~ "Higher Education",
    TRUE ~ NA_character_ ),levels =c("No Education", "Primary", "Secondary", "Higher Education"))) 

partnered_subset <- subset(women_with_children_w, Partnership_status == "Partnered")
lone_subset <- subset(women_with_children_w, C01001 == "Não")

svymean(x=~Settlement, design=lone_subset, na.rm=TRUE)

svymean(x=~J00101, design=partnered_subset, na.rm=TRUE)

svymean(x=~Age_Mother, design=subset(women_with_children_w, C01001=="Não"), na.rm=TRUE)

suppressWarnings({
  svymean(~J00101, design = subset(women_with_children_w, Partnership_status == "Lone"), na.rm = TRUE)
})


modeloLog <- svyglm(formula=SRH_binary~ C01001+C009+C008, design=women_with_children_weighted, family="binomial")


states <- read_state(
  code_state = "all",
  year = 2010,
  simplified = TRUE,
  showProgress = TRUE
)



# Make sure the identifiers match between the shapefile and your data
name_state$states <- as.character(name_state$states)  # Convert to character if necessary
data$state <- as.character(data$state)  # Convert to character if necessary

uf_data_2022 <- data.frame(
  name_state = c(
    "Rondônia", "Acre", "Amazonas", "Roraima", "Pará", 
    "Amapá", "Tocantins", "Maranhão", "Piauí", "Ceará", 
    "Rio Grande Do Norte", "Paraíba", "Pernambuco", "Alagoas", "Sergipe", 
    "Bahia", "Minas Gerais", "Espirito Santo", "Rio De Janeiro", "São Paulo",
    "Paraná", "Santa Catarina", "Rio Grande Do Sul", "Mato Grosso Do Sul", 
    "Mato Grosso", "Goiás", "Distrito Federal"
  ),
  Percentage_2022 = c(
    11.9, 16.7, 17.1, 16.0, 16.2, 
    18.6, 15.0, 16.9, 17.1, 17.6, 
    18.0, 16.1, 16.6, 17.7, 19.9, 
    17.9, 15.0, 13.7, 15.6, 14.2, 
    12.0, 10.6, 11.8, 13.7, 
    11.8, 13.7, 15.6
  )
)



# Merge the shapefile with your data
brazil_map_data <- left_join(states, uf_data_2022, by = "name_state")

# Plot the map
library(ggplot2)
library(sf)  # Ensure sf is loaded for geom_sf

ggplot(brazil_map_data) +
  geom_sf(aes(fill = Percentage_2022), color = "white") +
  scale_fill_gradient(low = "white", high = "#286D97FF", limits = c(10, 20)) +  # Adjust color to your preference
  labs(fill = "Percentage (%)") +
  theme_minimal() +  # Starts with a minimal theme as a base
  theme(
    panel.background = element_rect(fill = "white", colour = "white"),  # Set the panel background to white
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x = element_blank(),  # Optionally hide x axis text
    axis.text.y = element_blank(),  # Optionally hide y axis text
    axis.ticks = element_blank(),  # Optionally hide axis ticks
    plot.background = element_rect(fill = "white", colour = "white")  # Ensure the plot background is also white
  )





"#A63603" "#7F2704" "#FFF5EB" "#FEE6CE" "#FDD0A2" "#FDAE6B" "#FD8D3C" "#F16913" "#D94801"

