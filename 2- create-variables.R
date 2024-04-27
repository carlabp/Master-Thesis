
#create variables 

#rename age 
women_with_children <- women_with_children %>%
  rename(Age = C008)

#lone mother 
women_with_children <- women_with_children %>% 
  mutate(Partnership_status = ifelse(C01001 == "Não", "Lone", "Partnered"))

women_with_children$Partnership_status <- as.factor(women_with_children$Partnership_status)


#self reported health (all five categories)
women_with_children <- women_with_children %>% 
  mutate(SRH = case_when(
    J00101 == "Muito bom" ~ 1,
    J00101 == "Bom" ~ 2,
    J00101 == "Regular" ~ 3,
    J00101 == "Ruim" ~ 4,
    J00101 == "Muito ruim" ~ 5,
    TRUE ~ NA_integer_ 
  ))

# Race collapsed 
women_with_children <- women_with_children %>% 
  mutate(Race = case_when(
    C009 == "Branca" ~ "White",
    C009 == "Preta" | C009 == "Parda" | C009 == "Amarela" | C009 == "Indígena" | C009 == "Ignorado" ~ "Non-white",
    TRUE ~ NA_character_ 
  ))

women_with_children$Race <- as.factor(women_with_children$Race)

#Settlement type
women_with_children <- women_with_children %>% 
  mutate(Settlement = case_when(
    V0026 == "Urbano" ~ "Urban",
    V0026 == "Rural"  ~ "Rural",
    TRUE ~ NA_character_ 
  ))

women_with_children$Settlement <- as.factor(women_with_children$Settlement)


#self reported health (collapsed)
women_with_children <- women_with_children %>% 
  mutate(SRH_binary = case_when(
    J00101 == "Muito bom" | J00101 == "Bom" ~ 1,
    J00101 == "Regular" | J00101 == "Ruim" |  J00101 == "Muito ruim" ~ 0,
    TRUE ~ NA_integer_ 
  ))

women_with_children$SRH_binary <- as.numeric(women_with_children$SRH_binary)

#Region
women_with_children <- women_with_children %>%
  mutate(Region = case_when(
    V0001 %in% c("Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo") ~ "Southeast",
    V0001 %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina") ~ "South",
    V0001 %in% c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins") ~ "North",
    V0001 %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe") ~ "Northeast",
    V0001 %in% c("Goiás", "Mato Grosso", "Mato Grosso do Sul", "Distrito Federal") ~ "Mid-West",
    TRUE ~ NA_character_ 
  ))

women_with_children$Region <- as.factor(women_with_children$Region)


#Education (collapse categories)

#self reported health (collapsed)
women_with_children <- women_with_children %>% 
  mutate(Education = case_when(
    VDD004A == "Fundamental incompleto ou equivalente" | VDD004A == "Sem instrução" ~ "No Education",
    VDD004A == "Médio incompleto ou equivalente" | VDD004A == "Fundamental completo ou equivalente" ~ "Primary",
    VDD004A == "Superior incompleto ou equivalente" | VDD004A == "Médio completo ou equivalente" ~ "Secondary",
    VDD004A == "Superior completo" ~ "Higher Education",
    TRUE ~ NA_character_ 
  ))

women_with_children$Education <- as.factor(women_with_children$Education)


#Age of the Mother

hist(women_with_children$Age)

women_with_children <- women_with_children %>%
  mutate(Age_Mother = case_when(
    Age < 30 ~ "Less than 30",
    Age >= 30 & Age <= 39 ~ "30 to 39",
    Age >= 40 ~ "40 or older",
    TRUE ~ NA_character_
  )) %>%
  mutate(Age_Mother = as.factor(Age_Mother))


#employment (employed, unemployed, not employed)

women_with_children <- women_with_children %>%
  mutate(emprego = case_when(
    !is.na(VDE002) ~ VDE002, # When VDE002 is not NA, copy VDE002
    is.na(VDE002) ~ VDE001,  # When VDE002 is NA, copy VDE001
    TRUE ~ NA_character_     # Fallback in case of unexpected conditions
  ))

#employment dummy 
women_with_children <- women_with_children %>% 
  mutate(Employment = case_when(
    emprego == "Pessoas Ocupadas" ~ "Employed",
    emprego == "Pessoas desocupadas" | emprego == "Pessoas fora da força de trabalho" ~ "Unemployed/Not Employed",
    TRUE ~ NA_character_  
  ))

women_with_children$Employment <- as.factor(women_with_children$Employment)


#Create subset for lone mothers 

lone_mothers <- women_with_children %>%
  filter(Partnership_status == "Lone")

#Create subset for coupled mothers 

couple_mothers <- women_with_children %>%
  filter(Partnership_status == "Partnered")
