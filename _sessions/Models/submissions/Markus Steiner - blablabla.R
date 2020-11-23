require(tidyverse)
require(ggthemes)

# Erstelle plot
mein_plot <- read_csv('1_Data/Tourismus.csv') %>%
  inner_join(read_csv('1_Data/Europa.csv')) %>%
  ggplot(aes(x = Besucher, 
             y = Dauer, 
             color = Land)) + 
  geom_point() + 
  facet_wrap(~Erwerbsquote < 70) +
  theme_excel()

# Speichere plot als png
ggsave(filename = 'plot_DOZENTEN.png',
       plot = mein_plot,
       device = "png",
       units = "in",
       width = 7,
       height = 5)
