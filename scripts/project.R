install.packages("tidyverse")
library(tidyverse)
library(scales)
# Read Data
cybercrime_data <- read_csv("Cybercrime.csv")
cybercrime_data

# Calculating percent change over years
cyber_crime <- mutate(cybercrime_data,percent_change = (Total_violations - lag(Total_violations))/lag(Total_violations)*100,percent_change = replace_na(percent_change, 0))
cyber_crime$percent_change

# Scaling the values of % change to fit in graph with total_violations 
scale_factor <- max(cyber_crime$Total_violations) /
  max(abs(cyber_crime$percent_change))
scale_factor

library(ggplot2)
ggplot(cyber_crime, aes(x=factor(Year))) + 
  geom_col(aes(y = Total_violations),fill = "#7293C7", width = 0.6) +
  geom_line(aes(y = percent_change * scale_factor, group = 1),
            color = "#E23B3B", size = 1.1) +
  geom_point(aes(y = percent_change * scale_factor),
             color = "#E23B3B", size = 2)+
  geom_text(aes(y = percent_change * scale_factor,
                label = paste0(round(percent_change,1), "%")),
            vjust = -.7,hjust=1.4, color = "red", size = 4) +
  scale_y_continuous(
    name = "Total Cybercrime Violations",
    labels = comma,
    sec.axis = sec_axis(
      trans = ~ . / scale_factor,
      name  = "Percent Change (%)" )) +
        theme_minimal() +
        labs(
          title = "Total Cybercrime & Year-over-Year % Change (2015-2024)",
          x = "Year"
        ) +
        theme(
          axis.title.y.right = element_text(color = "red"),
          axis.text.y.right = element_text(color = "red")
        )
  
  

ggsave("Graph1.jpg",width = 7,height = 6)


#Plot2 

cybercrime_data

crime_vars <- c("Fraud",
                "Identity theft",
                "Identity fraud",
                "Extortion",
                "Criminal harassment")

crimes<-cybercrime_data %>%
  select(Year, all_of(crime_vars))
crimes

cyber_long <- crimes %>%
  pivot_longer(
    cols = -Year,
    names_to  = "Crime_Type",
    values_to = "Count"
  )
cyber_long
ggplot(cyber_long,
       aes(x = factor(Year),
           y = Count,
           colour = Crime_Type,
           group = Crime_Type)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  labs(
    title = "Trends in 5 Major Cybercrime Types (2015–2024)",
    x     = "Year",
    y     = "Number of Incidents",
    colour = "Crime Type"
  ) +
  theme_minimal() +
  theme(
    plot.title   = element_text(hjust = 0.5, face = "bold"),
    axis.text    = element_text(face = "bold"),
    axis.title   = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave("Graph2.jpg",width = 7,height = 6)

#plot 3
dataset <- read_csv("province_wise.csv")
dataset

transformed <- dataset %>% pivot_longer(cols = -Year,            # all columns except Year
                         names_to = "Province",   # new column with province names
                         values_to = "Incidents")

a<-filter(transformed,Year==2024)
a

library(scales) 

ggplot(a, aes(x = Incidents, y = reorder(Province, Incidents), fill = Incidents)) +
  geom_col(width = 0.8) +
  
  # Percentage labels
  geom_text(aes(label = sprintf("%.1f%%", Incidents / sum(Incidents) * 100)),
            hjust = -0.1,
            size = 3.8,
            color = "blue",
            fontface = "bold") +
  
  scale_fill_gradient(low = "#cfe8fb", high = "#08306b") +
  
  labs(
    title = "Cybercrime Incidents and % Share by Province",
    x = "Number of Incidents",
    y = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.y = element_text(face = "bold")
  ) +
  xlim(0, max(a$Incidents) * 1.20)
  


ggsave("Graph3.jpg",width = 7,height = 6)