library(ratdat)
library(tidyverse)


ggplot(data = complete_old, aes(x = weight, y = hindfoot_length, color = plot_type))+
  geom_point(alpha = 0.2, color = "blue")

ggplot(data = complete_old, aes(x = weight, y = hindfoot_length, color = plot_type))+
  geom_point(alpha = 0.2)+
  scale_color_viridis_d()+
  theme_bw()

ggplot(data = complete_old, aes(x = weight, y = hindfoot_length, color = plot_type))+
  geom_point(alpha = 0.2)+
  scale_x_log10()+
  scale_y_log10()

species_count <- complete_old |>
  group_by(species) |>
  summarize(count = n()) |>
  arrange(desc(count))|>
  slice(1:5)

complete_old |>
  filter(species %in% species_count$species) |>
ggplot(aes(x = plot_type, y = hindfoot_length, fill = plot_type))+
  geom_jitter(aes(color = plot_type), alpha = 0.2)+
  geom_boxplot()+
  scale_x_discrete(labels = label_wrap_gen(width = 10))+
  theme_bw()+
  theme(axis.title = element_text(size = 14, color = "purple"),
        legend.position = "none")+
  labs(title = "Rodent size by plot type",
       x = "Plot type",
       y = "Hindfoot length (mm)")+
  facet_wrap(vars(sex), nrow = 1)

complete_old |>
  filter(species %in% species_count$species) |>
ggplot(aes(x = plot_type, 
           y = hindfoot_length, 
           fill = plot_type))+
  geom_jitter(aes(color = plot_type), 
              alpha = 0.2)+
  geom_boxplot()+
  scale_x_discrete(labels = label_wrap_gen(width = 10))+
  theme_bw()+
  theme(axis.title = element_text(size = 14, 
                                  color = "purple"),
        legend.position = "none")+
  labs(title = "Rodent size by plot type",
       x = "Plot type",
       y = "Hindfoot length (mm)")+
  facet_grid(rows =  vars(sex), 
             cols = vars(species))

