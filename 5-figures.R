

#enrollment in nursery---------------------------------------------------------
nursery <- data.frame(
  Region = c("Brazil", "North", "Northeast", "Central-West", "Southeast", "South"),
  Percentage_2012 = c(14.8, 3.8, 8.7, 12.5, 20.9, 22.9),
  Percentage_2022 = c(35.9, 16.9, 30.2,28.3, 45.5, 42.4)
)

# Reshape data to long format
data_long <- reshape2::melt(nursery, id.vars = "Region", variable.name = "Year", value.name = "Percentage")

colors <- c("#3288BD",  "#F46D43")


ggplot(data_long, aes(x = Region, y = Percentage, fill = Year)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f", Percentage)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 5.0)  +
  scale_fill_manual(values = colors,
                    labels = c("2012", "2022"), 
                    name = "Year") +
  theme_light() +
  labs( y = "Percentage (%)") +
  theme(
    legend.position = "none", # Remove legend from individual plot
    text = element_text(size = 12), # Base text size; affects all text elements
    axis.title = element_text(size = 14, face = "bold"), 
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(hjust = 1), # Tilted x axis text
    title = element_text(size = 16, face = "bold") # Plot title
  ) +
  scale_y_continuous(limits = c(0, 100))


#enrollment in preschool--------------------------------------------------------

preschool <- data.frame(
  Region = c("Brazil", "North", "Northeast", "Central-West", "Southeast", "South"),
  Percentage_2012 = c(54.4, 45, 57.6, 47.1, 57.2, 51.1),
  Percentage_2022 = c(74.6, 64.5, 69, 71.7, 80.5, 80.9)
)

# Reshape data to long format
preschool_long <- reshape2::melt(preschool, id.vars = "Region", variable.name = "Year", value.name = "Percentage")

colors <- c("#3288BD",  "#F46D43")


ggplot(preschool_long, aes(x = Region, y = Percentage, fill = Year)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  
  geom_text(aes(label = sprintf("%.1f", Percentage)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("#3288BD",  "#F46D43"),
                    labels = c("2012", "2022"),
                    name = "Year") +
  theme_light() +
  labs(y = "Percentage (%)") +
  theme(
    legend.position = "none", 
    text = element_text(size = 12), 
    axis.title = element_text(size = 14, face = "bold"), 
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(hjust = 1), 
    title = element_text(size = 16, face = "bold") 
  ) +
  scale_y_continuous(limits = c(0, 100))

# to make a combined plot
combined_plot <- nursery_plot + preschool_plot + plot_layout(guides = 'collect')&theme(legend.position = "bottom")

#Access to childcare by race---------------------------------------------------

race<- data.frame(
  Age = c("0 to 1", "2 to 3", "4 to 5"),
  Percentage_black = c(87.1, 48.9, 8.9),
  Percentage_nonblack = c(83.7, 41.6, 7.9)
)
# Reshape data to long format
race_long <- reshape2::melt(race, id.vars = "Age", variable.name = "Race",
                            value.name = "Percentage")


 ggplot(race_long, aes(x = Age, y = Percentage, fill = Race)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.7)) +
  geom_text(aes(label = sprintf("%.1f", Percentage)), 
            position = position_dodge(width = 0.8), vjust = -0.5, size = 5) +
  scale_fill_manual(values = c("darkgrey", "darkorange"),
                    labels = c("Non-Black", "Black"),
                    name = "Race") +
  theme_light() +
  labs(x = "Age", y = "Percentage") +
  theme(
    text = element_text(size = 20), # Base text size; affects all text elements
    axis.title = element_text(size = 14, face = "bold"), # Axis titles
    axis.text = element_text(size = 14), # Axis text
    axis.text.x = element_text( hjust = 1), # Tilted x axis text
    title = element_text(size = 16, face = "bold") # Plot title
  )

#percentage of lone mothers by race --------------------------------------------
evolution_mothers <- data.frame(
  Year = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020",
           "2021", "2022"),
  White = c(4.1, 4.3, 4.3, 4.3, 4.1, 4.0, 4.1, 4.1, 4.3, 4.2, 4.3),
  Black = c(5.4, 5.5, 5.6,5.7, 6.0, 6.2, 6.2, 6.4, 6.8, 6.6, 6.9)
)

evolution_mothers <- evolution_mothers %>% 
  mutate(total = White + Black)

# Reshape data to long format for plotting
long_evolution_mothers <- reshape2::melt(evolution_mothers, id.vars = 'Year', 
                                         variable.name = 'Category', value.name = 'Value')

ggplot(long_evolution_mothers, aes(x = Year, y = Value, fill = Category)) + 
  geom_bar(data = subset(long_evolution_mothers, Category != 'Total'), stat = "identity", position = "dodge") +
  geom_line(data = subset(long_evolution_mothers, Category == 'Total'), aes(group = 1, colour = Category), size = 1) +
  geom_point(data = subset(long_evolution_mothers, Category == 'Total'), aes(colour = Category), size = 3) +
  scale_fill_manual(values = c("White" = "lightblue", "Black" = "darkmagenta", "Total" = "transparent")) +
  scale_color_manual(values = c("Total" = "black")) +
  labs(x = NULL, y = "Millions", fill = "Category", color = "Category", 
       title = "Evolução do número de pessoas de referência que são mães solo. Brasil.") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom") +
  # Create a secondary axis that's a transformation of the primary
  scale_y_continuous(sec.axis = sec_axis(~ . / 2, name = "Total"))


ggplot(long_evolution_mothers, aes(x = Year, y = Value, fill = Category)) + 
  geom_bar(stat = "identity") +
  labs(x = "Year", y = "Millions", fill = "Race", 
       title = "Evolution of the number of solo mothers by race") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom") +
  scale_fill_manual(values = c("White" = "lightblue", "Black" = "darkmagenta"))


long <- long_evolution_mothers %>% 
  group_by(Year) %>%
  arrange(Year, desc(Category)) %>%
  mutate(label_y = cumsum(Value) - (0.5 * Value))

# Create the stacked bar plot with values as text
ggplot(long, aes(x = Year, y = Value, fill = Category)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Value, y = label_y), vjust = 0.5, color = "black") +
  labs(x = "Year", y = "Millions", fill = "Race") +
  theme_minimal() +
  theme(axis.text.x = element_text(hjust = 1), 
        plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom") +
  scale_fill_manual(values = c("Black" = "grey", "White" = "#FDBE85"))


# Plot self assessed health---------------------------------------------------- 

health <- data.frame(
  Health = c("Very Poor", "Poor", "Regular", "Good", "Very Good"),
  All = c(0.5, 2.7, 17.4, 59.7, 19.7 ),
  Lone = c(1.1, 3.6, 21.2, 56.3, 17.8),  
  Couple = c(0.4,2.5, 16.5,60.5, 20.1)
)

health_long <- reshape2::melt(health, id.vars = "Health", variable.name = "Category",
                              value.name = "Percentage")

colors <- c("#3288BD", "#F46D43","#ABDDA4")

# Create the bar plot with renamed legends
ggplot(health_long, aes(x = factor(Health, levels = c("Very Good", "Good",
"Regular", "Poor", "Very Poor")), y = Percentage, fill = Category)) + 
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = sprintf("%.1f", Percentage)), position = position_dodge(width = 0.9), vjust = -0.5, size = 5) +
  scale_fill_manual(values = colors,
                    labels = c("All", "Lone Mothers", "Partnered Mothers"), # Rename legends here
                    name = "Partnership Status") + # Legend title
  theme_light() +
  labs(x = "Self Reported Health", y = "Percentage (%)") +
  theme(
    text = element_text(size = 12), 
    axis.title = element_text(size = 14, face = "bold"), 
    axis.text = element_text(size = 12), 
    axis.text.x = element_text(hjust = 1), 
    title = element_text(size = 16, face = "bold") 
  ) +
  scale_y_continuous(limits = c(0, 100))


col <- brewer.pal(10, name = "Blues")
col

# stacked option 

family_arrangement <- data.frame(
  Category = c("Couple with children", "Couple without children", "Lone mother with children",
           "Lone father with children", "One-person", "Other"),
  Percentage = c(40.2, 19, 14.7, 2.3, 16.5, 7.1)
)

family_long <- reshape2::melt(family_arrangement, id.vars = "Category", variable.name = "Year", value.name = "Percentage")

long <- family_long %>% 
  group_by(Year) %>%
  arrange(Year, desc(Category)) %>%
  mutate(label_y = cumsum(Percentage) - (0.5 * Percentage))

ggplot(long, aes(x = Year, y = Percentage, fill = Category)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Percentage, y = label_y), vjust = 0.5, color = "black", size =6) +
  labs(x = "Year", y = "Percentage", fill = "Category") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 14), 
    axis.text.y = element_text(size = 14), 
    axis.title = element_text(size = 16), 
    plot.title = element_text(hjust = 0.5, size = 25), 
    legend.position = "bottom",
    legend.text = element_text(size = 12), 
    text = element_text(size = 12) 
  )  +
  scale_x_discrete(labels = c("Year")) + 
  scale_fill_brewer(palette = "Spectral") +
  coord_flip()


ggplot(long, aes(x = Percentage, fill = Category)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = Percentage, x = label_y), vjust = 0.5, color = "black", size =6) +
  labs(x = "Percentage", fill = "Category") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(hjust = 1, size = 14), 
    axis.text.y = element_text(size = 14), 
    axis.title = element_text(size = 16), 
    plot.title = element_text(hjust = 0.5, size = 25), 
    legend.position = "bottom",
    legend.text = element_text(size = 12), 
    text = element_text(size = 12) 
  )  +
  scale_x_discrete(labels = c("Year")) + 
  scale_fill_brewer(palette = "Spectral") +
  coord_flip()


# Original color
original_color <- "#3288BD"

# to calculate a darker shade by reducing brightness
darker_shade <- adjustcolor(original_color, red = 0.8, green = 0.8, blue = 0.8)

spectral_colors = brewer.pal(11, "Spectral")
spectral_colors


#Marriage and divorce----------------------------------------------------------

marriage_divorce <- data.frame(
  Year = c("2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019", "2020",
           "2021", "2022"),
  Marriage = c( ,932502,970041),
  Divorce = c(174747, 239070, 347583, 341600,324921,341181,328960,344526,373216, 385246, 383286,331185,381813, 340359)
)

#Custody------------------------------------------------------------------------

custody <- data.frame(
  Year = c("2014", "2015", "2016", "2017", "2018", "2019", "2020",
           "2021", "2022"),
  Father = c(5.5,5.2,4.9,4.8,4.3,4.1,4.1, 2.6, 3.3),
  Mother = c(85.1, 79.2, 74.4, 69.4, 65.4, 62.4, 57.3, 54.2, 50.3), 
  Both = c(7.5, 12.9, 16.9, 20.9, 24.4, 26.8, 31.3, 34.5, 37.8)
)

long_data <- reshape2::melt(custody, id.vars = 'Year', variable.name = 'Category', value.name = 'Percentage')


# Plot
ggplot(long_data, aes(x = Year, y = Percentage, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  scale_color_manual(values = c ("#66C2A5", "red", "blue")) +
  theme_minimal() +
  labs(
    x = "Percentage (%)",
    y = "Year",
    color = "Category"
  ) +
  geom_text(aes(label = Percentage), hjust = -0.5, vjust = 0) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text( hjust = 1)
  )
long_data$label_nudge <- ifelse(long_data$Category == 'Men', -0.5, 
                         ifelse(long_data$Category == 'Women', 0.5, 
                                0.2))
ggplot(long_data, aes(x = Year, y = Percentage, color = Category, group = Category)) +
  geom_line(size = 1) +
  geom_point(size = 3) +
  geom_text_repel(
    aes(label = Percentage),
    nudge_y = long_data$label_nudge,  # Use the nudge values from the data frame
    size = 5,
    point.padding = 0.5,
    segment.color = 'grey50'
  ) +
  scale_color_manual(values = c("#66C2A5", "#D53E4F", "#3288BD")) +
  theme_minimal() +
  labs(
    y = "Percentage (%)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(hjust = 1, size = 14), 
    axis.text.y = element_text(size = 16), 
    legend.position = "bottom", 
    legend.title = element_blank(), 
    legend.text = element_text(size = 16)  
  )


#Map - proportion of households headed by lone mothers by Brazilian state-------

states <- read_state(
  code_state = "all",
  year = 2010,
  simplified = TRUE,
  showProgress = TRUE
)

name_state$states <- as.character(name_state$states) 
data$state <- as.character(data$state) 

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


brazil_map_data <- left_join(states, uf_data_2022, by = "name_state")

# Plot the map
ggplot(brazil_map_data) +
  geom_sf(aes(fill = Percentage_2022), color = "white") +
  scale_fill_gradient(low = "white", high = "#286D97FF", limits = c(10, 20)) + 
  labs(fill = "Percentage (%)") +
  theme_minimal() +  
  theme(
    panel.background = element_rect(fill = "white", colour = "white"), 
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    axis.text.x = element_blank(),  
    axis.text.y = element_blank(),  
    axis.ticks = element_blank(),  
    plot.background = element_rect(fill = "white", colour = "white")  
  )

