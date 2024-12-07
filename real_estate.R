getwd()
setwd("C:\\Users\\Ivan\\Documents\\DataAnalyst\\Bulgarian Real Estate Prices")
library(tidyverse)
library(readxl)
library(lubridate)
library(viridis)
library(scales)

data_row <- read_excel(file.choose())
#mutate dates to Date
data <- data_row%>%
  mutate(year=as.Date(year,format='%d.%m.%Y'))



#pivot long
growth_df <- data%>%
  pivot_longer(cols=c(inflation_index,
               real_estate_index,
               average_salary_index),
               names_to = 'category',
               values_to = 'growth'
               )


View(growth_df)




ggplot(growth_df, aes(x = year, y = growth, fill = category)) +
  geom_area(
    data = subset(growth_df, category == "average_salary_index"),
    stat = "identity",
    position = position_dodge2(width=0.3,  preserve = "single")) +
  geom_area(
    data = subset(growth_df, category != "average_salary_index"),
    stat = "identity",
           position = position_dodge2(width=0.3,  preserve = "single")) +
  scale_fill_manual(
    values = c( "average_salary_index"='red',
                "inflation_index"='blue',
                "real_estate_index"='green'),
    labels = c(
      "average_salary_index" = "Ръст на средната работна заплата",
      "inflation_index" = "Индекс на потребителските цени",
      "real_estate_index"="Индекс на цените на недвижимите имоти"
    )
  ) +
  scale_fill_viridis(
    alpha = 0.8,
    discrete = TRUE,
    option = "D",
    aesthetics = "fill",
    labels = c(
      "average_salary_index" = "Ръст на средната работна заплата",
      "inflation_index" = "Индекс на потребителските цени",
      "real_estate_index"="Индекс на цените на недвижимите имоти"
    )
  )+
  
  labs(
    title = "Сравнение на растежа на заплати, инфлация и цени на имоти",
    x = "Година",
    y = "Растеж, 2008 година = 100",
    fill = "Category",
    color = "Category"
  )+
  
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            panel.background = element_rect(fill = NA, color = NA))


# Step 1: Calculate the base year value (2010) for each category
base_year_values <- growth_df %>%
  filter(format(year, "%Y") == "2014") %>%
  group_by(category) %>%
  summarise(base_value = first(growth))

base_year_values

# Step 2: Rebase the data
rebased_df <- growth_df %>%
  left_join(base_year_values, by = "category") %>%
  mutate(rebased_growth = (growth / base_value) * 100)

View(rebased_df)
# Step 3: Filter for years after 2010 for plotting
growth_after_2014 <- rebased_df %>%
  filter(year >= as.Date("2014-01-01"))


View(growth_after_2014)

vir_fill <-  scale_color_viridis(
  alpha = 0.8,
  discrete = TRUE,
  option = "D",
  aesthetics = "colour"
)

max_index_value<- max(growth_after_2014$rebased_growth, na.rm = TRUE)



ggplot(data=growth_after_2014, aes(x = year, y = rebased_growth, colour = category)) +
  scale_y_continuous(
    limits = c(90,300),
    breaks = seq( 0, 280, # Use entire dataset
                  by = 50
    ),
    expand = c(0, 0) # Remove extra padding)
  ) +
  geom_line(
    data = subset(growth_after_2014, category == "average_salary_index"),
    stat = "identity",
    size=1,
    position = position_dodge2(width=0.3,  preserve = "single")) +
 
  geom_line(
    data = subset(growth_after_2014, category == "real_estate_index"),
    stat = "identity",
    size=1,
    position = position_dodge2(width=0.3,  preserve = "single")) +
  geom_line(
    data = subset(growth_after_2014, category == "inflation_index"),
    stat = "identity",
    size=1,
    position = position_dodge2(width=0.3,  preserve = "single")) +
 vir_fill +
  labs(
    title = "Сравнение на растежа на заплати, инфлация и цени на имоти",
    x = "Година",
    y = "Растеж, 2008 година = 100",
    fill = "Category",
    color = "Category"
  )+
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
            panel.background = element_rect(fill = NA, color = NA),
        panel.grid.major = element_line(color = "gray", size = 0.5),  # Major grid lines
        panel.grid.minor = element_blank()                            # Disable minor grid lines
        )+
  legend(c(""))

?geom_line

?legend
