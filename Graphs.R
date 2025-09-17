### Data transformations for the graphs

long_bias_data_base <- bias_data %>%
  pivot_longer(cols = starts_with("rating_") & -rating_AVG,
               names_to = "newspaper",
               names_prefix = "rating_",
               values_to = "rating")

long_bias_data <- long_bias_data_base %>% 
  mutate(newspaper = recode(newspaper, 
                            'C' = 'Corriere', 
                            'G' = 'Gazzetta', 
                            'T' = 'Tuttosport'))

long_bias_data_with_avg <- bias_data %>% 
  pivot_longer(cols = starts_with("rating_"),
               names_to = "newspaper",
               names_prefix = "rating_",
               values_to = "rating") %>%
  mutate(newspaper = recode(newspaper,
                            'AVG' = 'Average',
                            'C' = 'Corriere', 
                            'G' = 'Gazzetta', 
                            'T' = 'Tuttosport'))

ggplot(long_bias_data, aes(x = rating, fill = factor(Dblack), colour = factor(Dblack))) +
  geom_density(size = 1, alpha = 0.2) +
  scale_fill_manual(name = 'Group',
                      values = c('0' = 'blue', '1' = 'red'),
                      labels = c('0' = 'Non-Black', '1' = 'Black')) +
  scale_colour_manual(name = 'Group',
                        values = c('0' = 'blue', '1' = 'red'),
                        labels = c('0' = 'Non-Black', '1' = 'Black')) +
  labs(title = 'Kernel Densities - Player Ratings - Black and Non-Black by Newspaper',
       x = 'Rating', y = 'Density', color = 'Group', linetype = 'Group') +
  xlim(3,NA) +
  facet_wrap(~ newspaper) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 11))






  

##### Graphs #####

# Kernel Density Ratings: Black - Non-Black
ggplot(bias_data, aes(x = rating_AVG, color = factor(Dblack), linetype = factor(Dblack))) +
  geom_density(size = 1) + 
  scale_color_manual(values = c('blue', 'red'),
                     labels = c('Non-Black Players', 'Black Players')) +
  scale_linetype_manual(values = c('solid', 'dashed'),
                        labels = c('Non-Black Players', 'Black Players')) +
  labs(title = 'A: Kernel Densities - Player Ratings - Black and Non-Black', x = 'Rating', y ='', color = 'Group', linetype = 'Group') +
  xlim(3,NA) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = 'white'))

# Kernal Density Ratings: Non-Black by newspaper
non_black_filtered <- bias_data %>% filter(Dblack == 0)

non_black_by_newspaper <- ggplot() +
  geom_density(data = non_black_filtered, aes(x = rating_G, color = 'Gazzetta', linetype = 'Gazzetta'), size = 1) +
  geom_density(data = non_black_filtered, aes(x = rating_C, color = 'Corriere', linetype = 'Corriere'), size = 1) +
  geom_density(data = non_black_filtered, aes(x = rating_T, color = 'TuttoSport', linetype = 'TuttoSport'), size = 1) +
  scale_colour_manual(name = 'Newspaper',
                      values = c('Gazzetta' = 'blue','Corriere' = 'red','TuttoSport' = 'green')) +
  scale_linetype_manual(values = c('Gazzetta' = 'solid','Corriere' = 'dashed','TuttoSport' = 'longdash')) +
  labs(title = 'C: Kernel Densities - Player Ratings - Non-Black by Newspaper', x = 'Rating', y = '', color = 'Newspaper', linetype = 'Newspaper') +
  xlim(3, NA) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = 'white'))


# Kernal Density Ratings: Black by newspaper
black_filtered <- bias_data %>% filter(Dblack == 1)

black_by_newspaper <- ggplot() +
  geom_density(data = black_filtered, aes(x = rating_G, color = 'Gazzetta', linetype = 'Gazzetta'), size = 1) +
  geom_density(data = black_filtered, aes(x = rating_C, color = 'Corriere', linetype = 'Corriere'), size = 1) +
  geom_density(data = black_filtered, aes(x = rating_T, color = 'TuttoSport', linetype = 'TuttoSport'), size = 1) +
  scale_colour_manual(name = 'Newspaper',
                      values = c('Gazzetta' = 'blue','Corriere' = 'red','TuttoSport' = 'green')) +
  scale_linetype_manual(values = c('Gazzetta' = 'solid','Corriere' = 'dashed','TuttoSport' = 'longdash')) +
  labs(title = 'D: Kernel Densities - Player Ratings - Black by Newspaper', x = 'Rating', y = '', color = 'Newspaper', linetype = 'Newspaper') +
  xlim(3, NA) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = 'white'))

# Kernel Density Wages: Black - Non-Black
ggplot(bias_data, aes(x = lnwage, color = factor(Dblack), linetype = factor(Dblack))) +
  geom_density(size = 1) + 
  scale_color_manual(values = c('blue', 'red'),
                     labels = c('Non-Black Players', 'Black Players')) +
  scale_linetype_manual(values = c('solid', 'dashed'),
                        labels = c('Non-Black Players', 'Black Players')) +
  labs(title = 'B: Kernel Densities - Log Wages - Black and Non-Black', x = 'Log Wage', y ='', color = 'Group', linetype = 'Group') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = 'white'))


# ECDF by Newspaper
ggplot(long_bias_data_with_avg, aes(x = rating, colour = as.factor(Dblack), linetype = as.factor(Dblack))) +
  stat_ecdf(geom = 'step') +
  xlim(4, 7) +
  scale_colour_manual(name = 'Group',
                      values = c('0' = 'blue', '1' = 'red'),
                      labels = c('0' = 'Non-Black', '1' = 'Black')) +
  scale_linetype_manual(name = 'Group',
                      values = c('0' = 'solid', '1' = 'dashed'),
                      labels = c('0' = 'Non-Black', '1' = 'Black')) +
  labs(title = 'ECDF - Player Ratings - Black and Non-Black by Newspaper', y = 'Empirical Cumulative Distribution Function',
       x = 'Rating') +
  facet_wrap(~ newspaper) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = 'white'),
        strip.text = element_text(size = 11))



# Parameter Estimates 
estimates <- data.frame(
  Estimate = c(-8.536169e-02, -0.0587943636, -0.0955590668, -0.1017316345, -0.004510711),
  Std_Error = c(0.0300689775, 0.0297888554, 0.0395987037, 0.0382715024, 0.0503405276),
  Models = c('Average Rating', 'Gazzetta', 'Corriere','TuttoSport','Log Wage')
)

ggplot(estimates, aes(x= rownames(estimates), y = Estimate, colour = Models)) +
  geom_point(position = position_dodge(width = 0.5), size = 2.5, stroke = 1) +
  geom_errorbar(aes(ymin = Estimate - 1.96 * Std_Error, ymax = Estimate + 1.96 * Std_Error),
                position = position_dodge(width = 0.5),
                width = 0.2, size = 0.75) +
  geom_hline(yintercept = 0, colour = 'red', linetype = 'solid') +
  labs(title = 'Parameter Estimates - Effect of Black on Ratings and Player Wage', x = '', y ='Parameter Estimate', color = 'Group', linetype = 'Group') +
  theme_minimal() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom',
        legend.text = element_text(size = 10),
        plot.background = element_rect(fill = 'white'),
        panel.background = element_rect(fill = 'white'),
        legend.background = element_rect(fill = 'white'))

