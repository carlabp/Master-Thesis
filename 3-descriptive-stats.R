#Sample statistics 

# Partnership Status
lone_percentage <- round(sum(women_with_children$lone_mother == 1) / nrow(women_with_children) * 100, 2)
couple_percentage <- round(sum(women_with_children$lone_mother == 0) / nrow(women_with_children) * 100, 2)

# Self-reported Health
verygood_percentage <- round(sum(women_with_children$SRH == 1) / nrow(women_with_children) * 100, 2)
good_percentage <- round(sum(women_with_children$SRH == 2) / nrow(women_with_children) * 100, 2)
regular_percentage <- round(sum(women_with_children$SRH == 3) / nrow(women_with_children) * 100, 2)
bad_percentage <- round(sum(women_with_children$SRH == 4) / nrow(women_with_children) * 100, 2)
verybad_percentage <- round(sum(women_with_children$SRH == 5) / nrow(women_with_children) * 100, 2)

#SHR Collapsed
good_binary_percentage <- round(sum(women_with_children$SRH_binary == 1) / nrow(women_with_children) * 100, 2)
bad_binary_percentage <- round(sum(women_with_children$SRH_binary == 0) / nrow(women_with_children) * 100, 2)

# Urban vs. Rural Residence
urban_percentage <- round(sum(women_with_children$V0026 == "Urbano") / nrow(women_with_children) * 100, 2)
rural_percentage <- round(sum(women_with_children$V0026 == "Rural") / nrow(women_with_children) * 100, 2)

#region

north <- round(sum(women_with_children$Region == 2) / nrow(women_with_children) * 100, 2)
northeast <- round(sum(women_with_children$Region == 3) / nrow(women_with_children) * 100, 2)
centralwest <- round(sum(women_with_children$Region == 5) / nrow(women_with_children) * 100, 2)
south <- round(sum(women_with_children$Region == 1) / nrow(women_with_children) * 100, 2)
southeast <- round(sum(women_with_children$Region == 4) / nrow(women_with_children) * 100, 2)

# Education of the Mother
noeducation_percentage <- round(sum(women_with_children$education == 1) / nrow(women_with_children) * 100, 2)
primary_percentage <- round(sum(women_with_children$education == 2) / nrow(women_with_children) * 100, 2)
second_percentage <- round(sum(women_with_children$education == 3) / nrow(women_with_children) * 100, 2)
higher_percentage <- round(sum(women_with_children$education == 4) / nrow(women_with_children) * 100, 2)

# Household Income VDF004
proportion_quarter_wage <- round(sum(women_with_children$VDF004 == "Até ¼ salário mínimo") / nrow(women_with_children) * 100, 2)
proportion_quarter_to_half_minimum_wage <- round(sum(women_with_children$VDF004 == "Mais de ¼ até ½ salário mínimo") / nrow(women_with_children) * 100, 2)
proportion_half_to_one_minimum_wage <- round(sum(women_with_children$VDF004 == "Mais de ½ até 1 salário mínimo") / nrow(women_with_children) * 100, 2)
proportion_one_to_two_minimum_wages <- round(sum(women_with_children$VDF004 == "Mais de 1 até 2 salários mínimos") / nrow(women_with_children) * 100, 2)
proportion_two_to_three_minimum_wages <- round(sum(women_with_children$VDF004 == "Mais de 2 até 3 salários mínimos") / nrow(women_with_children) * 100, 2)
proportion_three_to_five_minimum_wages <- round(sum(women_with_children$VDF004 == "Mais de 3 até 5 salários mínimos") / nrow(women_with_children) * 100, 2)
proportion_more_than_five_minimum_wages <- round(sum(women_with_children$VDF004 == "Mais de 5 salários mínimos") / nrow(women_with_children) * 100, 2)
proportion_not_applicable <- round(sum(women_with_children$VDF004 == "Não aplicável") / nrow(women_with_children) * 100, 2)

# Age of the Mother
proportion_30_less <- round(sum(women_with_children$Age_Mother == "Less than 30") / nrow(women_with_children) * 100, 2)
proportion_30_to_39 <- round(sum(women_with_children$Age_Mother == "30 to 39") / nrow(women_with_children) * 100, 2)
proportion_40_or_older <- round(sum(women_with_children$Age_Mother == "40 or older") / nrow(women_with_children) * 100, 2)

# Labor force status 
in_labor <- round(sum(women_with_children$VDE001 == "Pessoas na força de trabalho") / nrow(women_with_children) * 100, 2)
out_labor <- round(sum(women_with_children$VDE001 == "Pessoas fora da força de trabalho") / nrow(women_with_children) * 100, 2)

#Employment sector 

domestic <- round(sum(women_with_children$employment_sector == 1) / nrow(women_with_children) * 100, 2)
private <- round(sum(women_with_children$employment_sector == 2) / nrow(women_with_children) * 100, 2)
public <- round(sum(women_with_children$employment_sector == 3) / nrow(women_with_children) * 100, 2)
employer<- round(sum(women_with_children$employment_sector == 4) / nrow(women_with_children) * 100, 2)
self_employed <- round(sum(women_with_children$employment_sector == 5) / nrow(women_with_children) * 100, 2)
other <- round(sum(women_with_children$employment_sector == 6) / nrow(women_with_children) * 100, 2)
no_sector <- round(sum(women_with_children$employment_sector == 7) / nrow(women_with_children) * 100, 2)

#Employment status 

m_employed <- round(sum(women_with_children$employment == "Pessoas Ocupadas") / nrow(women_with_children) * 100, 2)
m_unemployed <- round(sum(women_with_children$employment == "Pessoas desocupadas") / nrow(women_with_children) * 100, 2)
m_not_employed <- round(sum(women_with_children$employment == "Pessoas fora da força de trabalho") / nrow(women_with_children) * 100, 2)

# Race
white_percentage <- round(sum(women_with_children$C009 == "Branca") / nrow(women_with_children) * 100, 2)
mixed_percentage <- round(sum(women_with_children$C009 == "Parda") / nrow(women_with_children) * 100, 2)
black_percentage <- round(sum(women_with_children$C009 == "Preta") / nrow(women_with_children) * 100, 2)

# Race collapsed
cwhite_percentage <- round(sum(women_with_children$Race == "White") / nrow(women_with_children) * 100, 2)
non_white_percentage <- round(sum(women_with_children$Race == "Non-white") / nrow(women_with_children) * 100, 2)


#sample statistics for lone mothers
# Self-reported health
lm_verygood_percentage <- round(sum(lone_mothers$SRH == 1) / nrow(lone_mothers) * 100, 2)
lm_good_percentage <- round(sum(lone_mothers$SRH == 2) / nrow(lone_mothers) * 100, 2)
lm_regular_percentage <- round(sum(lone_mothers$SRH == 3) / nrow(lone_mothers) * 100, 2)
lm_bad_percentage <- round(sum(lone_mothers$SRH == 4) / nrow(lone_mothers) * 100, 2)
lm_verybad_percentage <- round(sum(lone_mothers$SRH == 5) / nrow(lone_mothers) * 100, 2)

#SHR Collapsed
lm_binary_good_percentage <- round(sum(lone_mothers$SRH_binary == 1) / nrow(lone_mothers) * 100, 2)
lm_binary_bad_percentage <- round(sum(lone_mothers$SRH_binary == 0) / nrow(lone_mothers) * 100, 2)


# Urban or rural
lm_urban_percentage <- round(sum(lone_mothers$V0026 == "Urbano") / nrow(lone_mothers) * 100, 2)
lm_rural_percentage <- round(sum(lone_mothers$V0026 == "Rural") / nrow(lone_mothers) * 100, 2)

#Region
lm_north <- round(sum(lone_mothers$Region == 2) / nrow(lone_mothers) * 100, 2)
lm_northeast <- round(sum(lone_mothers$Region == 3) / nrow(lone_mothers) * 100, 2)
lm_centralwest <- round(sum(lone_mothers$Region == 5) / nrow(lone_mothers) * 100, 2)
lm_south <- round(sum(lone_mothers$Region == 1) / nrow(lone_mothers) * 100, 2)
lm_southeast <- round(sum(lone_mothers$Region == 4) / nrow(lone_mothers) * 100, 2)


# Education of the mother
lm_noeducation_percentage <- round(sum(lone_mothers$education == 1) / nrow(lone_mothers) * 100, 2)
lm_primary_percentage <- round(sum(lone_mothers$education == 2) / nrow(lone_mothers) * 100, 2)
lm_second_compl_percentage <- round(sum(lone_mothers$education == 3) / nrow(lone_mothers) * 100, 2)
lm_higher_incom_percentage <- round(sum(lone_mothers$education == 4) / nrow(lone_mothers) * 100, 2)

# Household Income VDF004
lm_proportion_quarter_wage <- round(sum(lone_mothers$VDF004 == "Até ¼ salário mínimo") / nrow(lone_mothers) * 100, 2)
lm_proportion_quarter_to_half_minimum_wage <- round(sum(lone_mothers$VDF004 == "Mais de ¼ até ½ salário mínimo") / nrow(lone_mothers) * 100, 2)
lm_proportion_half_to_one_minimum_wage <- round(sum(lone_mothers$VDF004 == "Mais de ½ até 1 salário mínimo") / nrow(lone_mothers) * 100, 2)
lm_proportion_one_to_two_minimum_wages <- round(sum(lone_mothers$VDF004 == "Mais de 1 até 2 salários mínimos") / nrow(lone_mothers) * 100, 2)
lm_proportion_two_to_three_minimum_wages <- round(sum(lone_mothers$VDF004 == "Mais de 2 até 3 salários mínimos") / nrow(lone_mothers) * 100, 2)
lm_proportion_three_to_five_minimum_wages <- round(sum(lone_mothers$VDF004 == "Mais de 3 até 5 salários mínimos") / nrow(lone_mothers) * 100, 2)
lm_proportion_more_than_five_minimum_wages <- round(sum(lone_mothers$VDF004 == "Mais de 5 salários mínimos") / nrow(lone_mothers) * 100, 2)
lm_proportion_not_applicable <- round(sum(lone_mothers$VDF004 == "Não aplicável") / nrow(lone_mothers) * 100, 2)

# Age of the Mother

lm_proportion_30_less <- round(sum(lone_mothers$Age_Mother == "Less than 30") / nrow(lone_mothers) * 100, 2)
lm_proportion_30_to_39 <- round(sum(lone_mothers$Age_Mother == "30 to 39") / nrow(lone_mothers) * 100, 2)
lm_proportion_40_or_older <- round(sum(lone_mothers$Age_Mother == "40 or older") / nrow(lone_mothers) * 100, 2)

# Labor force
lm_in_labor <- round(sum(lone_mothers$VDE001 == "Pessoas na força de trabalho") / nrow(lone_mothers) * 100, 2)
lm_out_labor <- round(sum(lone_mothers$VDE001 == "Pessoas fora da força de trabalho") / nrow(lone_mothers) * 100, 2)

#Employment status 
lm_employed <- round(sum(lone_mothers$employment == "Pessoas Ocupadas") / nrow(lone_mothers) * 100, 2)
lm_unemployed <- round(sum(lone_mothers$employment == "Pessoas desocupadas") / nrow(lone_mothers) * 100, 2)
lm_notemployed <- round(sum(lone_mothers$employment == "Pessoas fora da força de trabalho") / nrow(lone_mothers) * 100, 2)

# Race
lm_white_percentage <- round(sum(lone_mothers$C009 == "Branca") / nrow(lone_mothers) * 100, 2)
lm_mixed_percentage <- round(sum(lone_mothers$C009 == "Parda") / nrow(lone_mothers) * 100, 2)
lm_black_percentage <- round(sum(lone_mothers$C009 == "Preta") / nrow(lone_mothers) * 100, 2)

#Race collapsed
lm_cwhite_percentage <- round(sum(lone_mothers$Race == "White") / nrow(lone_mothers) * 100, 2)
lm_non_white_percentage <- round(sum(lone_mothers$Race == "Non-white") / nrow(lone_mothers) * 100, 2)


# Self-reported health for couple_mothers
cm_verygood_percentage <- round(sum(couple_mothers$SRH == 1) / nrow(couple_mothers) * 100, 2)
cm_good_percentage <- round(sum(couple_mothers$SRH == 2) / nrow(couple_mothers) * 100, 2)
cm_regular_percentage <- round(sum(couple_mothers$SRH == 3) / nrow(couple_mothers) * 100, 2)
cm_bad_percentage <- round(sum(couple_mothers$SRH == 4) / nrow(couple_mothers) * 100, 2)
cm_verybad_percentage <- round(sum(couple_mothers$SRH == 5) / nrow(couple_mothers) * 100, 2)

#SHR Collapsed
cm_binary_good_percentage <- round(sum(couple_mothers$SRH_binary == 1) / nrow(couple_mothers) * 100, 2)
cm__binary_bad_percentage <- round(sum(couple_mothers$SRH_binary == 0) / nrow(couple_mothers) * 100, 2)

# Urban vs. Rural for couple_mothers
cm_urban_percentage <- round(sum(couple_mothers$V0026 == "Urbano") / nrow(couple_mothers) * 100, 2)
cm_rural_percentage <- round(sum(couple_mothers$V0026 == "Rural") / nrow(couple_mothers) * 100, 2)
cm_north <- round(sum(couple_mothers$Region == 2) / nrow(couple_mothers) * 100, 2)
cm_northeast <- round(sum(couple_mothers$Region == 3) / nrow(couple_mothers) * 100, 2)
cm_centralwest <- round(sum(couple_mothers$Region == 5) / nrow(couple_mothers) * 100, 2)
cm_south <- round(sum(couple_mothers$Region == 1) / nrow(couple_mothers) * 100, 2)
cm_southeast <- round(sum(couple_mothers$Region == 4) / nrow(couple_mothers) * 100, 2)

# Education level of the mother for couple_mothers
cm_noeducation_percentage <- round(sum(couple_mothers$education == 1) / nrow(couple_mothers) * 100, 2)
cm_primary_percentage <- round(sum(couple_mothers$education == 2) / nrow(couple_mothers) * 100, 2)
cm_second_compl_percentage <- round(sum(couple_mothers$education == 3) / nrow(couple_mothers) * 100, 2)
cm_higher_incom_percentage <- round(sum(couple_mothers$education == 4) / nrow(couple_mothers) * 100, 2)

# Household Income VDF004 for couple_mothers
cm_proportion_quarter_wage <- round(sum(couple_mothers$VDF004 == "Até ¼ salário mínimo") / nrow(couple_mothers) * 100, 2)
cm_proportion_quarter_to_half_minimum_wage <- round(sum(couple_mothers$VDF004 == "Mais de ¼ até ½ salário mínimo") / nrow(couple_mothers) * 100, 2)
cm_proportion_half_to_one_minimum_wage <- round(sum(couple_mothers$VDF004 == "Mais de ½ até 1 salário mínimo") / nrow(couple_mothers) * 100, 2)
cm_proportion_one_to_two_minimum_wages <- round(sum(couple_mothers$VDF004 == "Mais de 1 até 2 salários mínimos") / nrow(couple_mothers) * 100, 2)
cm_proportion_two_to_three_minimum_wages <- round(sum(couple_mothers$VDF004 == "Mais de 2 até 3 salários mínimos") / nrow(couple_mothers) * 100, 2)
cm_proportion_three_to_five_minimum_wages <- round(sum(couple_mothers$VDF004 == "Mais de 3 até 5 salários mínimos") / nrow(couple_mothers) * 100, 2)
cm_proportion_more_than_five_minimum_wages <- round(sum(couple_mothers$VDF004 == "Mais de 5 salários mínimos") / nrow(couple_mothers) * 100, 2)
cm_proportion_not_applicable <- round(sum(couple_mothers$VDF004 == "Não aplicável") / nrow(couple_mothers) * 100, 2)

# Age of the Mother for couple_mothers
cm_proportion_30_less <- round(sum(couple_mothers$Age_Mother == "Less than 30") / nrow(couple_mothers) * 100, 2)
cm_proportion_30_to_39 <- round(sum(couple_mothers$Age_Mother == "30 to 39") / nrow(couple_mothers) * 100, 2)
cm_proportion_40_or_older <- round(sum(couple_mothers$Age_Mother == "40 or older") / nrow(couple_mothers) * 100, 2)


# Working status for couple_mothers
cm_in_labor <- round(sum(couple_mothers$VDE001 == "Pessoas na força de trabalho") / nrow(couple_mothers) * 100, 2)
cm_out_labor <- round(sum(couple_mothers$VDE001 == "Pessoas fora da força de trabalho") / nrow(couple_mothers) * 100, 2)

#Employment status
cm_employed <- round(sum(couple_mothers$employment == "Pessoas Ocupadas") / nrow(couple_mothers) * 100, 2)
cm_unemployed <- round(sum(couple_mothers$employment == "Pessoas desocupadas") / nrow(couple_mothers) * 100, 2)
cm_notemployed <- round(sum(couple_mothers$employment == "Pessoas fora da força de trabalho") / nrow(couple_mothers) * 100, 2)

# Race for couple_mothers
cm_white_percentage <- round(sum(couple_mothers$C009 == "Branca") / nrow(couple_mothers) * 100, 2)
cm_mixed_percentage <- round(sum(couple_mothers$C009 == "Parda") / nrow(couple_mothers) * 100, 2)
cm_black_percentage <- round(sum(couple_mothers$C009 == "Preta") / nrow(couple_mothers) * 100, 2)


#Race collapsed 
cm_cwhite_percentage <- round(sum(couple_mothers$Race == "White") / nrow(couple_mothers) * 100, 2)
cm_non_white_percentage <- round(sum(couple_mothers$Race == "Non-white") / nrow(couple_mothers) * 100, 2)


#number of children 
cm_number_children <- round(mean(couple_mothers$number_children),1)
lm_number_children <- round(mean(lone_mothers$number_children),1)
number_children <- round(mean(women_with_children$number_children),1)


#Create table: sociodemographic variables of lone and partnered mothers without N 

sample_table1 <- data.frame(
  Variables = c( "Good", "Bad", "Urban", "Rural", "South", "Southeast",
                 "Mid-West", "Northeast", "North",
                 "White", "Non-white",
                 "Below 18", 
                 "18 to 29", "30 to 49", "50 years or older",
                 "No Education", "Primary", 
                 "Secondary",
                 "Superior", 
                 "Employed", "Unemployed", "Not Employed", 
 "Yes", "No", "Number of Children", "Less than 40h", "40h", "More than 40h", "Unemployed/not employed"),
  Total = c(good_binary_percentage,
             bad_binary_percentage,
             urban_percentage,
             rural_percentage,
             south,
             southeast,
             centralwest,
             northeast,
             north,
             cwhite_percentage,
             non_white_percentage,
             Below18,
             proportion_18_to_29,
             proportion_30_to_49,
             proportion_50_or_older,
             noeducation_percentage,
             primary_percentage,
             second_percentage, 
             higher_percentage,
             m_employed, 
             m_unemployed,
             m_not_employed,
             young_child, no_youngchild, number_children, less_40, forty, more_40,
            zero_hour),
  Lone = c(
    lm_binary_good_percentage,
    lm_binary_bad_percentage,
    lm_urban_percentage, 
    lm_rural_percentage,
    lm_south,
    lm_southeast,
    lm_centralwest,
    lm_northeast,
    lm_north,
    lm_white_percentage,
    lm_non_white_percentage,
    lm_Below18,
    lm_proportion_18_to_29,
    lm_proportion_30_to_49,
    lm_proportion_50_or_older,
    lm_noeducation_percentage,
    lm_primary_percentage,
    lm_second_compl_percentage, 
    lm_higher_incom_percentage, 
    lm_employed,
    lm_unemployed,
    lm_notemployed,
    lm_young_child,
    lm_no_young_child, 
    lm_number_children, lm_less_40, lm_forty, lm_more_40, lm_zero_hour
  ), 
  Couple = c(
    cm_binary_good_percentage,
    cm__binary_bad_percentage,
    cm_urban_percentage,
    cm_rural_percentage,
    cm_south,
    cm_southeast,
    cm_centralwest,
    cm_northeast,
    cm_north,
    cm_cwhite_percentage,
    cm_non_white_percentage,
    cm_Below18,
    cm_proportion_18_to_29,
    cm_proportion_30_to_49,
    cm_proportion_50_or_older,
    cm_noeducation_percentage,
    cm_primary_percentage,
    cm_second_compl_percentage, 
    cm_higher_incom_percentage,
    cm_employed, 
    cm_unemployed, 
    cm_notemployed, 
    cm_young_child, 
    cm_no_young_child,
    cm_number_children, cm_less_40, cm_forty, cm_more_40, cm_zero_hour
  )
)

sample_table1 %>%
  kbl() %>%
  kable_classic(full_width = F, html_font = "Arial")  %>%
  pack_rows("Self Reported Health", 1, 2) %>%
  pack_rows("Settlemente type", 3, 4) %>%
  pack_rows("Region", 5, 9) %>%
  pack_rows("Ethnicity", 10, 12) %>%
  pack_rows("Mother's age", 13, 16) %>%
  pack_rows("Mother's education level", 17, 20) %>%
  pack_rows("Employment", 21, 23) %>%
  pack_rows("Children 3 years or younger", 24, 25) 


sample_table1 <- gt(sample_table1)

sample_table1 %>%
  tab_header(
    title = md("**Distribution of the analytical sample by sociodemographic aspects**")
  ) %>%
  tab_row_group(
    label = "Education Level",
    rows = 10:13
  ) %>%
  tab_row_group(
    label = "Mother's Age",
    rows = 6:9
  ) %>%
  tab_row_group(
    label = "Ethnicity",
    rows = 3:5
  ) %>%
  tab_row_group(
    label = "Territory",
    rows = 1:2
  ) %>%
  tab_spanner(
    label = "Percentage (%)",
    columns = c(Mothers, Lone, Couple)
  )


#Create table: Labor 

sample_table2 <- data.frame(
  Variables = c("In the labor force", "Out of the labor force", 
                 "Employed", "Unemployed", "Out of the labor force", 
                  "Less than 40 hours", "40 hours", 
                 "More than 40 hours", "Not employed/unemployed"),
  Mothers = c(in_labor,
              out_labor,
              employed,
              unemployed,
              not_employed, 
              less_40,
              forty,
              more_40,
              zero_hour
              ),
  Lone = c(
  lm_in_labor,
  lm_out_labor, 
  lm_employed,
  lm_unemployed, 
  lm_notemployed, 
  lm_less_40,
  lm_forty,
  lm_more_40, 
  lm_zero_hour
  ), 
  Couple = c(
 cm_in_labor,
 cm_out_labor,
 cm_employed, 
 cm_unemployed, 
 cm_notemployed, 
 cm_less_40,
 cm_forty, 
 cm_more_40, 
 cm_zero_hour
  )
)

sample_table2 <- gt(sample_table2)

sample_table2 %>%
  tab_header(
    title = md("**Descriptive statistics: employment and labor market integration**")
  ) %>%
  tab_row_group(
    label = "Average work hours per week",
    rows = 6:9
  ) %>%
  tab_row_group(
    label = "Employment status",
    rows = 3:5
  ) %>%
  tab_row_group(
    label = "Labor force status",
    rows = 1:2
  )


#Create table: sociodemographic variables of lone and partnered mothers with N

sample_table1 <- data.frame(
  Variables = c( "Urban", "Rural", 
                "White", "Mixed", "Black", 
                 "Below 18", 
                "18 to 29", "30 to 49", "50 years or older",
                "No Education", "Primary", 
                "Secondary",
                "Superior"),
  Mothers = c(urban_percentage, rural_percentage, white_percentage,
              mixed_percentage, black_percentage, Below18, proportion_18_to_29, proportion_30_to_49,
              proportion_50_or_older, noeducation_percentage, primary_percentage, second_percentage, 
              higher_percentage),
  N = c(n_urban, n_rural, n_white, n_mixed, n_black, n_Below18, n_18_to_29, n_30_to_49, n_50_or_older,
        n_noeducation, n_primary, n_second_, n_higher), 
  Lone = c(
    lm_urban_percentage, 
    lm_rural_percentage,
    lm_white_percentage,  
    lm_mixed_percentage,  
    lm_black_percentage, 
    lm_Below18,
    lm_proportion_18_to_29,
    lm_proportion_30_to_49,
    lm_proportion_50_or_older,
    lm_noeducation_percentage,
    lm_primary_percentage,
    lm_second_compl_percentage, 
    lm_higher_incom_percentage
    ), 
  N = c(n_lm_urban, 
  n_lm_rural, 
  n_lm_white, 
  n_lm_mixed,
  n_lm_black, 
  n_lm_Below18,
  n_lm_18_to_29,
  n_lm_30_to_49,
  n_lm_50_or_older, 
  n_lm_noeducation,
  n_lm_primary,
  n_lm_second,
  n_lm_higher), 
  Couple = c(
    cm_urban_percentage,
    cm_rural_percentage,
    cm_white_percentage,
    cm_mixed_percentage,
    cm_black_percentage,
    cm_Below18,
    cm_proportion_18_to_29,
    cm_proportion_30_to_49,
    cm_proportion_50_or_older,
    cm_noeducation_percentage,
    cm_primary_percentage,
    cm_second_compl_percentage, 
    cm_higher_incom_percentage
  ), 
  N = c(n_cm_urban, 
        n_cm_rural, 
        n_cm_white, 
        n_cm_mixed,
        n_cm_black, 
        n_cm_Below18,
        n_cm_18_to_29,
        n_cm_30_to_49,
        n_cm_50_or_older, 
        n_cm_noeducation,
        n_cm_primary,
        n_cm_second,
        n_cm_higher) 
)

sample_table1 <- gt(sample_table1)

sample_table1 %>%
  tab_header(
    title = md("**Distribution of the analytical sample by sociodemographic factors**")
  ) %>%
  tab_row_group(
    label = "Education Level",
    rows = 10:13
  ) %>%
  tab_row_group(
    label = "Mother's Age",
    rows = 6:9
  ) %>%
  tab_row_group(
    label = "Ethnicity",
    rows = 3:5
  ) %>%
  tab_row_group(
    label = "Territory",
    rows = 1:2
  ) %>%
  tab_spanner(
    label = "Percentage (%)",
    columns = c(Mothers, Lone, Couple)
  )

