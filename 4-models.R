

#relevel the variable partnership status
women_with_children$Partnership_status <- relevel(women_with_children$Partnership_status, 
                                                  ref="Partnered")
#relevel the variable Region 
women_with_children$Region <- relevel(women_with_children$Region, 
                                                  ref="South")

#relevel the variable Settlement 
women_with_children$Settlement <- relevel(women_with_children$Settlement, 
                                      ref="Urban")

#relevel the variable race
women_with_children$Race <- relevel(women_with_children$Race, 
                                          ref="White")

#relevel the variable Employment 
women_with_children$Employment <- relevel(women_with_children$Employment, 
                                                  ref="Unemployed/Not Employed")

#relevel the variable Education
women_with_children$Education <- relevel(women_with_children$Education, 
                                          ref="No Education")

#Relevel the variable Mother's age
women_with_children$Age_Mother <- relevel(women_with_children$Age_Mother, 
                                                  ref="Less than 30")

#Relevel the variables for weighted regression
women_with_children_w <- women_with_children_w %>%
  mutate(Partnership_status = relevel(Partnership_status, ref = "Partnered"))%>%
  mutate(Region = relevel(Region, ref = "South")) %>%
  mutate(Education = relevel(Education, ref = "No Education"))  %>%
  mutate(Employment = relevel(Employment, ref = "Unemployed/Not Employed"))  %>%
  mutate(Age_Mother = relevel(Age_Mother, ref = "Less than 30"))  %>%
  mutate(Race = relevel(Race, ref = "White"))  %>%
  mutate(Settlement = relevel(Settlement, ref = "Urban"))


#create a variable for the interaction 
women_with_children$Interaction <- women_with_children$Partnership_status:women_with_children$Employment

#relevel the variable interaction  
women_with_children$Interaction <- relevel(women_with_children$Interaction, 
                                           ref="Lone:Unemployed/Not Employed") 


#Model 1: partnership status + mother's age + race + settlement  
model1 <- glm(SRH_binary ~ Partnership_status + Age_Mother + Race + Settlement + Region,
              data = women_with_children, family = binomial)

#Model 2: partnership status + mother's age + race + settlement +  education 
model2 <- glm(SRH_binary ~ Partnership_status + Age_Mother + Race + Settlement + Region +
                Education, data = women_with_children, family = binomial)

#Model 3: partnership status + mother's age + race + settlement +  education + employment 
model3 <- glm(SRH_binary ~ Partnership_status + Age_Mother + Race + Settlement + Region +
                Education + Employment, data = women_with_children, family = binomial)

#Model 4: Interaction partnership status + mother's age + race + settlement +  education + employment + 
#partnership:employment

model4 <- glm(SRH_binary ~ Interaction + Age_Mother + Settlement + Race + Education + Region,
              data = women_with_children, family = binomial)

#Table 

table1 <- tbl_regression(model1, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table2 <- tbl_regression(model2, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table3 <- tbl_regression(model3, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table4 <- tbl_regression(model4, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))


tbl_merge(
  tbls = list(table1, table2, table3, table4),
  tab_spanner = c("**Model 1**", "**Model 2**", "**Model 3**", 
                  "**Model 4**"))%>% 
  modify_header(label="**Variable**") %>%
  modify_table_styling(
    column = c(ci_1, ci_2, ci_3, ci_4),
    hide = TRUE) %>%
as_gt() %>%
  tab_footnote(
    footnote = "Model 1 accounts for sociodemographic controls (mother’s age, mother’s race, region, and settlement type). 
  Model 2 adds employment status. Model 3 also includes educational attainment. Model 4 adds an interaction 
    between partnership status and employment status",
    placement = c("left"))


#Tables for the appendix 

#Model 1: partnership status + mother's age + race + settlement  
model1 <- glm(SRH_binary ~ Partnership_status + Age_Mother + Race + Settlement + Region,
              data = women_with_children, family = binomial)

model1_weighted <- svyglm(formula= SRH_binary ~ Partnership_status + Age_Mother + Race + Settlement + 
                            Region, design=women_with_children_w, family="binomial")

#Model 2: partnership status + mother's age + race + settlement +  education 
model2 <- glm(SRH_binary ~ Partnership_status + Age_Mother + Race + Settlement + Region +
                Education, data = women_with_children, family = binomial)

model2_weighted <- svyglm(formula= SRH_binary ~ Partnership_status + Age_Mother + Race + Settlement + 
                            Region + Education, design=women_with_children_w, family="binomial")


#Model 3: partnership status + mother's age + race + settlement +  education + employment 
model3 <- glm(SRH_binary ~ Partnership_status + Age_Mother + Race + Settlement + Region +
                Education + Employment, data = women_with_children, family = binomial)

model3_weighted <- svyglm(formula= SRH_binary ~ Partnership_status + Age_Mother + Race + Settlement + 
                            Region + Education + Employment, design=women_with_children_w, family="binomial")



#Model 4: Interaction partnership status + mother's age + race + settlement +  education + employment + 
#partnership:employment

model4 <- glm(SRH_binary ~ Interaction + Age_Mother + Settlement + Race + Education + Region,
              data = women_with_children, family = binomial)

model4_weighted <- svyglm(formula= SRH_binary ~ Partnership_status*Employment + Partnership_status + Employment + Age_Mother + Race + Settlement + 
                            Region + Education, design=women_with_children_w, family="binomial")


#Test

modelweighted <- svyglm(formula=J00101 ~ C01001 + C008 + C009 + V0026, design=women_with_children_weighted, family="binomial")


table1 <- tbl_regression(model1, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table1_weighted <- tbl_regression(model1_weighted, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))



table2 <- tbl_regression(model2, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table2_weighted <- tbl_regression(model2_weighted, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table3 <- tbl_regression(model3, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table3_weighted <- tbl_regression(model3_weighted, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table4 <- tbl_regression(model4, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table4_weighted <- tbl_regression(model4_weighted, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))





tbl_merge(
  tbls = list(table1, table1_weighted, table2, table2_weighted),
  tab_spanner = c("**Model 3 - Unweighted**", "**Model 3 - Weighted**", "**Model 4 - Unweighted**", "**Model 4 - Weighted**" )) %>% 
  modify_header(label="**Variable**") 


tbl_merge(
  tbls = list(table3, table3_weighted, table4, table4_weighted),
  tab_spanner = c("**Model 3 - Unweighted**", "**Model 3 - Weighted**", "**Model 4 - Unweighted**", "**Model 4 - Weighted**")) %>% 
  modify_header(label="**Variable**") 




#Table 

table1 <- tbl_regression(model1, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table2 <- tbl_regression(model1_weighted, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table3 <- tbl_regression(model2, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table4 <- tbl_regression(model2_weighted, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table5 <- tbl_regression(model3, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))
table6 <- tbl_regression(model3_weighted, exponentiate = TRUE, label = c(Partnership_status = "Partnership Status", Age_Mother = "Mother's Age"))

# Merge the tables
tbl_merge(
  tbls = list(table1, table2, table3, table4, table5, table6),
  tab_spanner = c("**Model 1**", "**Model 1 weighted**", "**Model 2**", 
                  "**Model 2 weighted**", "**Model 3**", "**Model 3 weighted**")
) %>% 
  modify_header(label="**Variable**") %>%
  as_gt() %>%
tab_footnote(
  footnote = "Model 1 accounts for sociodemographic controls (mother’s age, mother’s race, region, and settlement type). 
  Model 2 adds employment status. Model 3 also includes educational attainment",
  placement = c("left"))

tbl_merge(
  tbls = list(table5),
  tab_spanner = c("**Model 4**")) %>% 
  modify_header(label="**Variable**") %>%
  as_gt()


#Log likelihood test
lrtest(model8)


#model for lone mothers 

#create a variable for the interaction 
lone_mothers$Interaction <- lone_mothers$Education:lone_mothers$Employment

#relevel the variable interaction  
lone_mothers$Interaction <- relevel(lone_mothers$Interaction, 
                                           ref="No Education:Unemployed/Not Employed") 



#Predicted probabilities 


women_with_children$Partnership_status = factor(women_with_children$Partnership_status, levels = c(
  "1-Lone"
  ,
  "2-Partnered"
))

women_with_children$Employment = factor(women_with_children$Employment, levels = c(
  "1-Employed"
  ,
  "2-Unemployed/Not Employed"
))



model5 <- glm(SRH_binary ~ Partnership_status:Employment  + Age_Mother + Settlement +
                Race + Education + Region, data = women_with_children, family = binomial)

plot_model(model5, type =
             "pred"
           , terms = c(
             
             "Employment",
             "Partnership_status"
           ))


plot_model(model5, 
           type = "pred", 
           terms = c( "Partnership_status", "Employment"),
           colors = c("#F46D43", "#3288BD"),
           title = "",  
           axis.title = c("Partnership Status", "Predicted Probability"), 
           legend.title = "Employment Status",
           theme = sjPlot::theme_sjplot()
)
library(forcats)

TEST01<-summary(model3)$coefficients

CI.vector <- as.data.frame(exp(confint(model3)))

TEST02 <- cbind(TEST01, CI.vector)
TEST03<-data.frame(TEST02)
TEST04<- TEST03[2:14,]
TEST04$Labels = c("Lone","Age 30-39","40 or older","White",
"Urban","Mid-West","North","Northeast", "Southeast", "Higher Education",
"Primary", "Secondary", "Employed")
TEST04$OR = exp(TEST04$Estimate)



TEST04$Labels <-fct_relevel(TEST04$Labels,
                            "Primary", 
                            "Secondary", 
                            "Higher Education", 
                            "Northeast",
                            "North",
                            "Mid-West",
                            "Southeast",
                            "Urban",
                            "White",
                            "40 or older",
                            "Age 30-39", 
                            "Employed", 
                            "Lone", 
                          )

ggplot(TEST04, aes(x = OR, y = Labels)) +
  geom_point(size =
               3.5
             , color =
               "#F46D43"
  )+
  geom_vline(aes(xintercept =
                   1
  ), size =
    .25
  , linetype =
    "dashed"
  ) +
  ylab(
    ""
  ) +
  xlab(
    "Odds Ratio"
  )+
  geom_errorbarh(aes(xmax = `X97.5..`, xmin = `X2.5..`, height =.1)) 


+
  
  
  labs(caption = "The reference categories are partnered for partnership status,
  unemployed/not employed, less than 30 years old, black, rural, south, no education")





+
    theme(plot.caption = element_text(hjust = 0, face = "italic", colour = "gray", size = 10)) +
   
    
   


