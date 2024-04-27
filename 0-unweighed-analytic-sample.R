library(PNSIBGE)
library(tidyverse)
library(gtsummary)
library(kableExtra)
library(knitr)
library(dplyr)
library(eha)
library(gt)
library(modelsummary)
library(ggplot2)
library(reshape2)
library(patchwork)

#Load data
pns_df <- get_pns(year=2019, vars=c( "V001", "V0026", "C004","C006","C008",
                                     "C009","C01001", "C01801", 
                                     "C011","C012", "C014","D00201","D00202", "E001", 
                                     "E002", "E003", "E004", "E005",
                                     "E011","E017", "E01401", "E014011", "E01402",
                                     "E01403", "E01201", "E01403", "J007", 
                                     "J00101", "VDD004A", 
                                     "VDE001", "VDF004", "VDE014", "VDE002", "S066", "J060",
                                     "Q092", "F007011", "VDE014"), design=F)

#Build the analytic sample 

# Exclude children that are older than 18
filtered_df <- subset(pns_df, 
                      !((C004 == "Filho(a) somente do responsável" & C008 >= 18) | 
                          (C004 == "Filho(a) somente do cônjuge" & C008 >= 18) | 
                          (C004 == "Filho(a) do responsável e do cônjuge" & C008 >= 18)))


#Identify children
children_df <- filtered_df %>%
  filter(C004 == "Filho(a) somente do responsável" | C004 == "Filho(a) somente do cônjuge" |
           C004 == "Filho(a) do responsável e do cônjuge" )


#List the distinct ID Domicilios of families with children younger than 18 years
ID_families_children <- children_df%>%
  distinct(ID_DOMICILIO)

#List all the household with children and its components 
families_w_children <- filtered_df %>%
  filter(ID_DOMICILIO %in% ID_families_children$ID_DOMICILIO)


#add variables related to children 

#if the household has children younger than 3 years old encoded 1, if not, 0 

families_w_children <- families_w_children %>%
  mutate(young_child_indicator = ifelse(C008 <= 3, 1, 0)) %>% #for each row, if 
  #the value is of C008 is less than or equal to 3, young_child_indicator is set to 1
  group_by(ID_DOMICILIO) %>%
  mutate(young_children = ifelse(any(young_child_indicator == 1), 1, 0))%>%
  ungroup()

# While the data is still grouped by ID_DOMICILIO, this line adds another new column called young_children
#to the data frame. For each group (household), if any row within the group has a young_child_indicator 
#equal to 1 (meaning there's at least one child aged 3 or younger in the household), then young_children is
#set to 1 for all rows in that group. If not, young_children is set to 0. This step effectively flags entire 
#households that have young children

#number of children 

# Identify children based on the given criteria
families_w_children <- families_w_children %>%
  mutate(Is_Child = case_when(
    C004 %in% c("Filho(a) somente do responsável", "Filho(a) do responsável e do cônjuge", "Filho(a) somente do cônjuge") ~ 1,
    TRUE ~ 0
  ))

# Sum the number of children per household
children_per_household <- families_w_children %>%
  group_by(ID_DOMICILIO) %>%
  summarise(Number_of_Children = sum(Is_Child))

#number of children 

# Identify children based on the given criteria
families_w_children <- families_w_children %>%
  mutate(Is_Child = case_when(
    C004 %in% c("Filho(a) somente do responsável", "Filho(a) do responsável e do cônjuge", "Filho(a) somente do cônjuge") ~ 1,
    TRUE ~ 0
  )) %>%
  group_by(ID_DOMICILIO) %>%
  mutate(number_children = sum(Is_Child)) %>%
  ungroup()

#Support 
families_w_children <- families_w_children %>%
  group_by(ID_DOMICILIO) %>%
  mutate(Support = ifelse(any(C004 %in% c("Genro ou nora", "Pai, mãe, padrasto ou madrasta", "Sogro(a)", 
                                          "Neto(a)", "Bisneto(a)", "Irmão ou irmã", "Avô ou avó", 
                                          "Outro parente", "Agregado(a) - Não parente que não compartilha despesas", 
                                          "Convivente - Não parente que compartilha despesas", "Pensionista", 
                                          "Empregado(a) doméstico(a)", "Parente do(a) empregado(a) doméstico(a)", 
                                          "Não aplicável")), 1, 0)) %>%
  ungroup()



#Analytic sample
#Filter all the women who are head or partner 
women_with_children <- families_w_children %>% 
  filter(C004 == "Cônjuge ou companheiro(a) de sexo diferente"|
           C004 == "Pessoa responsável pelo domicílio" |
           C004 ==  "Cônjuge ou companheiro(a) do mesmo sexo")


women_with_children <- women_with_children %>% 
  filter (C006 == "Mulher")

write.csv(sample_table1, "sampletable.csv", row.names = FALSE)


women <- pns_df  %>% 
  filter (C006 == "Mulher") %>% 
  filter (C009 == "Branca")


pns_df_weighted



