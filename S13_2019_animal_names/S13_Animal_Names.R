
library(tidyverse)
library(lubridate)
library(magick)
library(magrittr) 
library(ggthemes)

#CSV
seattle_pets <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-03-26/seattle_pets.csv")
head(seattle_pets)

#Data (date)
pets.data <- seattle_pets %>% 
  mutate(date.ins = mdy(license_issue_date),
         month.ins = month(date.ins),
         year.ins = year(date.ins))

head(pets.data)
str(pets.data)
names(pets.data)

#------------+
### 1) DOG ----
#------------+

#plot
(pets.plot <- pets.data %>% 
  filter(species=="Dog") %>% 
  group_by(animals_name) %>% 
  summarize(n=n()) %>% 
  arrange(desc(n)) %>% 
  top_n(15) %>% 
  ggplot(aes(reorder(animals_name, n),n, fill=n)) +
  geom_col()+
  scale_y_continuous(limits = c(0,400), breaks = seq(0,400,50), expand = c(0,0)) +
  geom_text(aes(label=n), hjust =-.5) +
  geom_hline(yintercept = 0, color="grey20", size=1) +
  theme_minimal()+
  scale_fill_viridis_c()+
  labs(title="TOP 15 SEATTLE DOG NAMES",
         subtitle = "Pets registered in 2018",
         caption = "@amadeob12",
       x="",
       y="count (n°)")+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text.x = element_blank(),
        legend.position = "none") +
  theme(plot.title = element_text(color = "grey30", size=14, face="bold"),
        plot.subtitle = element_text(color = "grey50", size=12),
        plot.caption = element_text(color = "grey60", vjust = 0.5, hjust = 0.95)) +
  coord_flip()
) 


ggsave(pets.plot, file="pets.png", width = 5, height = 4, dpi = 300)
plot <- image_read("pets.png")
plot


#Gif
dog_gif <- image_read ("https://media.giphy.com/media/l4FGI8GoTL7N4DsyI/giphy.gif")%>% 
  image_scale ("150")%>% 
  image_rotate (0) 
dog_gif


## Combinar gráfico y GIF

# seleccionar imagen de fondo
background <- image_background(image_scale(plot, "500"), "white", flatten = TRUE)
# Combinar
frames <- image_composite(background, dog_gif, offset = "+310+150")
# Animación
animation <- image_animate(frames, fps = 10)
print(animation)

#Save gif
image_write(animation, "petsgif1.gif")









#----------------------------------------------------------------------------------------------------------------------------------------------------------------------------+




#------------+
### 2) CAT ----
#------------+

#plot
(cat.plot <- pets.data %>% 
   filter(species=="Cat", animals_name!="NA") %>% 
   group_by(animals_name) %>% 
   summarize(n=n()) %>% 
   arrange(desc(n)) %>% 
   top_n(15) %>% 
   ggplot(aes(reorder(animals_name, n),n, fill=n)) +
   geom_col()+
   scale_y_continuous(limits = c(0,200), breaks = seq(0,400,50), expand = c(0,0)) +
   geom_text(aes(label=n), hjust =-.5) +
   geom_hline(yintercept = 0, color="grey20", size=1) +
   theme_minimal()+
   scale_fill_viridis_c(option="magma")+
   labs(title="TOP 15 SEATTLE CAT NAMES",
        subtitle = "Pets registered in 2018",
        caption = "@amadeob12",
        x="",
        y="count (n°)")+
   theme(panel.grid.minor.x = element_blank(),
         panel.grid.major.x = element_blank(),
         panel.grid.major.y = element_blank(),
         axis.text.x = element_blank(),
         legend.position = "none") +
   theme(plot.title = element_text(color = "grey30", size=14, face="bold"),
         plot.subtitle = element_text(color = "grey50", size=12),
         plot.caption = element_text(color = "grey60", vjust = 0.5, hjust = 0.95)) +
   coord_flip()
) 


ggsave(cat.plot, file="cat.png", width = 5, height = 4, dpi = 300)
plot2 <- image_read("cat.png")
plot2


#gif
cat_gif <- image_read ("https://media.giphy.com/media/4BJCvMoLPePq8/giphy.gif")%>% 
  image_scale ("130")%>% 
  image_rotate (0) 
cat_gif

# Combine the plot and animation
# seleccionar imagen de fondo
background_2 <- image_background(image_scale(plot2, "500"), "white", flatten = TRUE)
# Combinnar
frames_2 <- image_composite(background_2, cat_gif, offset = "+250+170")
# Animación
animation_2 <- image_animate(frames_2, fps = 5)
print(animation_2)

#Save gif
image_write(animation_2, "pcat_gif.gif")
