getwd()
setwd("C:\\Users\\Ivan\\Documents\\DataAnalyst\\Bulgarian Real Estate Prices")
library(tidyverse)
library(readxl)
library(lubridate)
library(viridis)
library(scales)

data_row <- read_excel(file.choose())
data <- data_row%>%
  mutate(year=as.Date(year,format='%d.%m.%Y'))

View(data)
str(data)

growth_df2 <- data%>%
  pivot_longer(cols=c(average_price_condo,
                      mortgage_payment,
                     ),
               names_to = 'category',
               values_to = 'value'
  )%>%
  mutate(
    # Scale `average_price_condo` to align with the primary y-axis (0–100)
    scaled_value = ifelse(category == "average_price_condo", value, value *200)
  )

# growth_number <- data%>%
#   pivot_longer(cols=c(average_price_condo,
#                       real_estate_index_growth,
#                       average_salary_growtth),
#                names_to = 'category',
#                values_to = 'growth'
#   )
View(growth_number)
?position_dodge

viridis= scale_fill_viridis(
  alpha = 0.8,
  begin = 0,
  end = 1,
  direction = 1,
  discrete = TRUE,
  option = "C",
)
viridisColor= scale_colour_viridis(
  alpha = 0.8,
  begin = 0,
  end = 1,
  direction = 1,
  discrete = TRUE,
  option = "C",
)

?scale_fill_viridis

# Plot with dual y-axes
ggplot(growth_df, aes(x = factor(year), fill = category)) +
  geom_bar(
    aes(y = scaled_value),
    stat = "identity",
    position = position_dodge2(width = 0.1, preserve = "single")
  ) +
  scale_x_discrete(
    breaks = c(
      '2008-12-31', '2009-12-31', '2010-12-31', '2011-12-31', '2012-12-31',
      '2013-12-31', '2014-12-31', '2015-12-31', '2016-12-31', '2017-12-31',
      '2018-12-31', '2019-12-31', '2020-12-31', '2021-12-31', '2022-12-31',
      '2023-12-31', '2024-03-31', '2024-06-30'
    ),
    expand = expansion(mult = c(0.01, 0.02)) # Ensure space for the quarterly points
  ) +
  scale_y_continuous(
    name = "Average Price Condo (in 1000 €)",  # Primary y-axis for `average_price_condo`
    limits = c(0,200000),
    sec.axis = sec_axis(~ . / 200,
                        name = "Mortgage Payment (€)",
                       )  # Secondary y-axis for `mortgage_payment`
  ) +
 
  scale_fill_viridis(
    alpha = 1,
    discrete = TRUE,
    option = "C",
    aesthetics = "fill"
  ) +
  labs(
    title = "Growth Comparison",
    x = "Year",
    fill = "Category"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.background = element_rect(fill = NA, color = NA),
    axis.title.y.right = element_text(color = "black")  # Secondary axis label visibility
  )

data$year <- as.Date(data$year) # Ensure the `year` column is in Date format

ggplot(data, aes(x = year)) +
  # Bar chart for real_estate_index_growth
  geom_bar(aes(y = real_estate_index_growth*100, fill = "Real Estate Index Growth"), 
           stat = "identity", position = "dodge", width = 200) + # Adjust `width` for bar spacing
  # Line chart for average_interest_rate_mortgage
  geom_line(aes(y = average_interest_rate_mortgage, color = "Average Interest Rate (Mortgage)"), size = 1) +
  geom_point(aes(y = average_interest_rate_mortgage , color = "Average Interest Rate (Mortgage)"), size = 2) +
  
  # Scales and secondary y-axis
  scale_y_continuous(
    name = "Real Estate Index Growth (%)",
    limits = c(-30, 20), # Set limits for the left y-axis
    breaks = seq(-30, 20, by = 5), # Define breaks for the left y-axis
    sec.axis = sec_axis(~./1, name = "Average Interest Rate (Mortgage, %)"),
    expand = expansion(mult = c(-0.1, 0.1)) # Adds some space for better display
  ) +
  
  # Formatting for the x-axis
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.02)) # Ensure space for the quarterly points
  ) +
  
  # Labels and titles
  labs(
    title = "Real Estate Growth vs Average Mortgage Interest Rate",
    x = "Year",
    fill = "Category",
    color = "Category"
  ) +
  
  # Custom colors for the charts
  scale_fill_manual(values = c("Real Estate Index Growth" = "#69b3a2")) +
  scale_color_manual(values = c("Average Interest Rate (Mortgage)" = "#404080")) +
  
  # Theme for better readability
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

# str(data)
# data_factor<- data%>%mutate(year=factor(year))
# str(data_factor)
# ggplot(data_factor, aes(x =year),) +
#   # Bar chart for real_estate_index_growth
#   geom_bar(aes(y = real_estate_index_growth*100, fill = "Real Estate Index Growth"), 
#            stat = "identity", position = position_dodge2(width=0.1,  preserve = "single")) + # Adjust `width` for bar spacing
#   # Line chart for average_interest_rate_mortgage
#   geom_line(aes(y = average_interest_rate_mortgage, color = "Average Interest Rate (Mortgage)"), size = 1,group = 1) +
#   geom_point(aes(y = average_interest_rate_mortgage , color = "Average Interest Rate (Mortgage)"), size = 2) +
#   
#   # Scales and secondary y-axis
#   scale_y_continuous(
#     name = "Real Estate Index Growth (%)",
#     limits = c(-30, 20), # Set limits for the left y-axis
#     breaks = seq(-30, 20, by = 5), # Define breaks for the left y-axis
#     sec.axis = sec_axis(~./1, name = "Average Interest Rate (Mortgage, %)"),
#     expand = expansion(mult = c(-0.1, 0.1)) # Adds some space for better display
#   ) +
#   
#   # Formatting for the x-axis
#   scale_x_discrete(
#     breaks = c('2008-12-31','2009-12-31','2010-12-31','2011-12-31','2012-12-31',
#                '2013-12-31','2014-12-31',
#                '2015-12-31','2016-12-31','2017-12-31',
#                '2018-12-31','2019-12-31','2020-12-31',
#                '2021-12-31','2022-12-31','2023-12-31',
#                '2024-03-31','2024-06-30'),
#     # date_labels =c(replicate(15,'%Y'),'%month','%month'),
#     expand = expansion(mult = c(0.01, 0.02)) # Ensure space for the quarterly points
#   ) +
#   
#   # Labels and titles
#   labs(
#     title = "Real Estate Growth vs Average Mortgage Interest Rate",
#     x = "Year",
#     fill = "Category",
#     color = "Category"
#   ) +
#   
#   # Custom colors for the charts
#   # scale_fill_manual(values = c("Real Estate Index Growth" = "#69b3a2")) +
#   # scale_color_manual(values = c("Average Interest Rate (Mortgage)" = "#404080")) +
#   scale_fill_viridis(
#     alpha = 0.5,
#     discrete = TRUE,
#     option = "C",
#     aesthetics = "fill"
#   )+
#   viridisColor+
#   
#   # Theme for better readability
#   # theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1),
#     legend.position = "bottom"
#   )
# 

# Plot with dual y-axes and line chart for `average_price_condo`
ggplot(growth_df, aes(x = factor(year))) +
  # Bar chart for `mortgage_payment`
  geom_bar(
    data = growth_df %>% filter(category == "average_price_condo"),
    aes(y = scaled_value, fill = category),
    stat = "identity",
    position = position_dodge2(width = 0.1, preserve = "single")
  ) +
  # Line chart for `mortgage_payment`
  geom_line(
    data = growth_df %>% filter(category == "mortgage_payment"),
    aes(y = scaled_value, color = category, group = 1),
    size = 1.2
  ) +
  # Optional: Add points to the line chart
  geom_point(
    data = growth_df %>% filter(category == "mortgage_payment"),
    aes(y = scaled_value, color = category),
    size = 2
  ) +
  scale_x_discrete(
    breaks = c(
      '2008-12-31', '2009-12-31', '2010-12-31', '2011-12-31', '2012-12-31',
      '2013-12-31', '2014-12-31', '2015-12-31', '2016-12-31', '2017-12-31',
      '2018-12-31', '2019-12-31', '2020-12-31', '2021-12-31', '2022-12-31',
      '2023-12-31', '2024-03-31', '2024-06-30'
    ),
    expand = expansion(mult = c(0.01, 0.02)) # Ensure space for the quarterly points
  ) +
  scale_y_continuous(
    name = "Average Price Condo (in 1000 €)",  # Primary y-axis for `average_price_condo`
    limits = c(0,200000),
    sec.axis = sec_axis(~ . / 200,
                        name = "Mortgage Payment (€)",
                       )  # Secondary y-axis for `mortgage_payment`
  ) +
  scale_fill_viridis(
    alpha = 0.8,
    discrete = TRUE,
    option = "C",
    aesthetics = "fill"
  ) +
  scale_color_viridis(
    alpha = 1,
    discrete = TRUE,
    option = "D",
    aesthetics = "color"
  ) +
  labs(
    title = "Growth Comparison",
    x = "Year",
    fill = "Category",
    color = "Category"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.background = element_rect(fill = NA, color = NA),
    axis.title.y.right = element_text(color = "black")  # Secondary axis label visibility
  )
