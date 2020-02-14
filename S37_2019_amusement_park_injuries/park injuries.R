tx_injuries <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/tx_injuries.csv")

safer_parks <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-09-10/saferparks.csv")
head(safer_parks)
str(safer_parks)

library(tidyverse)
library(lubridate)
library(magick)
library(magrittr) 
library(scales)


#Ver en que formato vienen las fechas
safer_parks$acc_date
class(safer_parks$acc_date)


rev_1 <- as.data.frame(str_split(safer_parks$acc_date,"/", simplify = TRUE))
head(rev_1)
str(rev_1)


#Plot general
(plot_1 <- safer_parks %>%
  mutate(acc_state = as.character(acc_state),
         date_ok = mdy(acc_date)) %>% 
  mutate(year_ok = year(date_ok),
         month_ok = month(date_ok)) %>% 
  filter(year_ok !="NA") %>% 
  group_by(month_ok) %>%
  summarise(n_inj = sum(num_injured, na.rm = TRUE)) %>%
  ungroup() %>%
      #GGPLOT2
  ggplot(aes(as.factor(month_ok), n_inj))+
  geom_col(fill="#30d59f", color="#30d59f", alpha=.6) +
  scale_y_continuous(breaks = seq(0,2000,400),limits = c(0,2000),expand = c(0.001,0.001),
                     minor_breaks = seq(0, 2000, 100))+
  labs(title = "\nMONTHLY INJURIES IN AMUSEMENT PARKS",
       subtitle="USA. 2010 - 2017\n",
       x="\nMonths",
       y="Injuries (n°)\n",
       caption = "by: @amadeob12 | source: https://saferparksdata.org/") +
  theme_minimal()+
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_line(color="grey23"),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color="grey30"),
        plot.background = element_rect(fill = "grey20"),
        plot.title = element_text(size=23, face = "bold", color="#30d59f"),
        plot.subtitle = element_text(size=15, color="grey80"),
        plot.caption = element_text(size=9, color="grey75"),
        axis.title.x = element_text(color = "grey85", size=12),
        axis.title.y = element_text(color = "grey85", size=12),
        axis.text = element_text(color = "grey80", size=10),
        axis.line.x = element_line(size=.8, color="grey80"),
        axis.ticks.x = element_line(color="grey80"))
)

ggsave(plot_1, file="park_general.png", width =10, height = 5, dpi = 600)


# try geofacet https://ryanhafen.com/blog/geofacet/
#install.packages("geofacet")
library(geofacet)

(park_g1 <- safer_parks %>%
  mutate(acc_state = as.character(acc_state),
         date_ok = mdy(acc_date)) %>% 
  mutate(year_ok = year(date_ok),
         month_ok = month(date_ok)) %>% 
  filter(year_ok !="NA") %>% 
  group_by(month_ok, acc_state) %>%
  summarise(n_inj = sum(num_injured, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(state = acc_state) %>%
        #GGPLOT2
  ggplot(aes(as.factor(month_ok), n_inj))+
  geom_col(fill="#30d59f", color="#30d59f", alpha=.7) +
  scale_y_continuous(expand = c(0.01,0.01))+
  geom_hline(yintercept = 0, color="grey70") +
  labs(title = "\nAmusement Park Injuries per state",
       subtitle="USA. 2010 - 2017",
       x="\nMonths",
       y="Injuries (n°)\n",
       caption = "Source: https://saferparksdata.org/ | by: @amadeob12") +
  facet_geo(~state) +
  theme_light()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        strip.background = element_rect(fill = "grey80"),
        plot.background = element_rect(fill = "grey20"),
        panel.background = element_rect(fill = "grey30"),
        plot.title = element_text(size=26, face = "bold", color="#30d59f"),
        plot.subtitle = element_text(size=18, color="grey80"),
        plot.caption = element_text(size=9, color="grey75"),
        axis.title.x = element_text(hjust = 0, color = "grey85", size=12),
        axis.title.y = element_text(hjust = 0, color = "grey85", size=12),
        axis.text = element_text(color = "grey80", size=8),
        axis.text.x = element_text(angle = 90, vjust = .5),
        strip.text = element_text(color="grey30"))
)

ggsave(park_g1, file="park_g1.png", width =15, height = 9, dpi = 600)

### Agregar GIF
#-----------------
#Cargar gráfico como imagen
plot <- image_read("park_g1.png")

#cargar Gif_1
park_gif_1 <- image_read ("https://media.giphy.com/media/xUOrvXXY1U0bbbYnyU/giphy.gif")%>% 
  image_scale ("150")%>% 
  image_rotate (0) 
park_gif_1

# seleccionar imagen de fondo (gráfico)
background <- image_background(image_scale(plot, "1200"), "white", flatten = TRUE)
# Combinar
frames <- image_composite(background, park_gif_1, offset = "+1040+450")
# Animación
animation <- image_animate(frames, fps = 20)
print(animation)
#Save gif
image_write(animation, "park_gif1.gif")




# Heatmap (individual)
(plot_3 <- safer_parks %>%
  mutate(acc_state = as.character(acc_state),
         date_ok = mdy(acc_date)) %>% 
  mutate(year_ok = year(date_ok), 
         month_ok = month(date_ok),
         date_2 = dmy(str_glue("01-{month_ok}-{year_ok}"))) %>% 
  filter(year_ok !="NA") %>% 
  group_by(acc_state, date_2) %>%
  summarise(n_inj = sum(num_injured, na.rm = TRUE)) %>%
  ungroup() %>%
  rename(state = acc_state) %>% 
    #GGPLOT2
  ggplot(aes(date_2, state, fill=n_inj))+
  geom_vline(xintercept = ymd("2010-12-16"), linetype=1, color="grey50") +
  geom_vline(xintercept = ymd("2011-12-16"), linetype=1, color="grey50") +
  geom_vline(xintercept = ymd("2012-12-16"), linetype=1, color="grey50") +
  geom_vline(xintercept = ymd("2013-12-16"), linetype=1, color="grey50") +
  geom_vline(xintercept = ymd("2014-12-16"), linetype=1, color="grey50") +
  geom_vline(xintercept = ymd("2015-12-16"), linetype=1, color="grey50") +
  geom_vline(xintercept = ymd("2016-12-16"), linetype=1, color="grey50") +
  geom_tile() +
  #scale_fill_viridis_c() +
  scale_fill_viridis_c(option = "B") +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("1 month"), expand = c(0.02,0.02)) +
  labs(title = "\nAMUSEMENT PARK INJURIES",
       subtitle="USA. 2010 - 2017\n",
       x="\nMonths",
       y="State\n",
       fill="Injuries (n°)",
       caption = "by: @amadeob12 | Source: https://saferparksdata.org/") +
  theme_minimal()+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "grey25", color = "grey25"),
        panel.background = element_rect(fill = "grey25", color = "grey25"),
        plot.title = element_text(size=25, face = "bold", color="#F8CE49"),
        plot.subtitle = element_text(size=15, color="grey85", face = "bold"),
        plot.caption = element_text(size=9, color="grey75"),
        text = element_text(color="grey85"),
        axis.title.x = element_text(color = "grey75", size=11),
        axis.title.y = element_text(color = "grey75", size=11),
        axis.text.y = element_text(color = "grey70", size=9),
        axis.text.x = element_text(color = "grey70", size=9, angle = 90, vjust = .5),
        legend.position = "bottom") +
  annotate("text", x = ymd("2010-08-01"), y = 20, label = "2010", color= "grey60", alpha=.2, size=24, angle=90) +
  annotate("text", x = ymd("2011-07-01"), y = 20, label = "2011", color= "grey60", alpha=.2, size=24, angle=90) +
  annotate("text", x = ymd("2012-07-01"), y = 20, label = "2012", color= "grey60", alpha=.2, size=24, angle=90) +
  annotate("text", x = ymd("2013-07-01"), y = 20, label = "2013", color= "grey60", alpha=.2, size=24, angle=90) +
  annotate("text", x = ymd("2014-07-01"), y = 20, label = "2014", color= "grey60", alpha=.2, size=24, angle=90)+
  annotate("text", x = ymd("2015-07-01"), y = 20, label = "2015", color= "grey60", alpha=.2, size=24, angle=90) +
  annotate("text", x = ymd("2016-07-01"), y = 20, label = "2016", color= "grey60", alpha=.2, size=24, angle=90) +
  annotate("text", x = ymd("2017-05-01"), y = 20, label = "2017", color= "grey60", alpha=.2, size=24, angle=90)
)

ggsave(plot_3, file="park_3.png", width =15, height = 9, dpi = 600)




# Heatmap + gráfico de barras (unir con: gridextra o cowplot)
(plot_3b <- safer_parks %>%
    mutate(acc_state = as.character(acc_state),
           date_ok = mdy(acc_date)) %>% 
    mutate(year_ok = year(date_ok), 
           month_ok = month(date_ok),
           date_2 = dmy(str_glue("01-{month_ok}-{year_ok}"))) %>% 
    filter(year_ok !="NA") %>% 
    group_by(acc_state, date_2) %>%
    summarise(n_inj = sum(num_injured, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(state = acc_state) %>% 
    #GGPLOT2
    ggplot(aes(date_2, state, fill=n_inj))+
    geom_vline(xintercept = ymd("2010-12-16"), linetype="dashed", color="grey50") +
    geom_vline(xintercept = ymd("2011-12-16"), linetype="dashed", color="grey50") +
    geom_vline(xintercept = ymd("2012-12-16"), linetype="dashed", color="grey50") +
    geom_vline(xintercept = ymd("2013-12-16"), linetype="dashed", color="grey50") +
    geom_vline(xintercept = ymd("2014-12-16"), linetype="dashed", color="grey50") +
    geom_vline(xintercept = ymd("2015-12-16"), linetype="dashed", color="grey50") +
    geom_vline(xintercept = ymd("2016-12-16"), linetype="dashed", color="grey50") +
    geom_tile() +
    #scale_fill_viridis_c() +
    scale_fill_viridis_c(option = "B") +
    scale_x_date(labels = date_format("%b"), breaks = date_breaks("1 month"), expand = c(0.02,0.02)) +
    scale_y_discrete(position = "right") +
    labs(title = "AMUSEMENT PARK INJURIES",
         subtitle="USA. 2010 - 2017\n",
         x="\nMonths",
         y="",
         fill="Injuries\n(n°)",
         caption = "") +
    theme_minimal()+
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_blank(),
          plot.background = element_rect(fill = "grey25", color = "grey25"),
          panel.background = element_rect(fill = "grey25", color = "grey25"),
          plot.title = element_text(size=25, face = "bold", color="#F8CE49"),
          plot.subtitle = element_text(size=15, color="grey85", face = "bold"),
          plot.caption = element_text(size=9, color="grey75"),
          text = element_text(color="grey85"),
          axis.title.x = element_text(color = "grey75", size=11),
          axis.title.y = element_text(color = "grey75", size=11),
          axis.text.y = element_text(color = "grey70", size=11),
          axis.text.x = element_text(color = "grey70", size=9, angle = 90, vjust = .5),
          legend.position = "left") +
    annotate("text", x = ymd("2010-08-01"), y = 20, label = "2010", color= "grey60", alpha=.2, size=24, angle=90) +
    annotate("text", x = ymd("2011-07-01"), y = 20, label = "2011", color= "grey60", alpha=.2, size=24, angle=90) +
    annotate("text", x = ymd("2012-07-01"), y = 20, label = "2012", color= "grey60", alpha=.2, size=24, angle=90) +
    annotate("text", x = ymd("2013-07-01"), y = 20, label = "2013", color= "grey60", alpha=.2, size=24, angle=90) +
    annotate("text", x = ymd("2014-07-01"), y = 20, label = "2014", color= "grey60", alpha=.2, size=24, angle=90)+
    annotate("text", x = ymd("2015-07-01"), y = 20, label = "2015", color= "grey60", alpha=.2, size=24, angle=90) +
    annotate("text", x = ymd("2016-07-01"), y = 20, label = "2016", color= "grey60", alpha=.2, size=24, angle=90) +
    annotate("text", x = ymd("2017-05-01"), y = 20, label = "2017", color= "grey60", alpha=.2, size=24, angle=90)
)


(plot_3.1 <- safer_parks %>%
    mutate(acc_state = as.character(acc_state),
           date_ok = mdy(acc_date)) %>% 
    mutate(year_ok = year(date_ok), 
           month_ok = month(date_ok),
           date_2 = dmy(str_glue("01-{month_ok}-{year_ok}"))) %>% 
    filter(year_ok !="NA") %>% 
    group_by(acc_state) %>%
    summarise(n_inj = sum(num_injured, na.rm = TRUE)) %>%
    ungroup() %>%
    rename(state = acc_state) %>% 
    #GGPLOT2
    ggplot(aes(state, n_inj, fill=n_inj))+
    geom_col(fill="#F8CE49", aes(alpha=ifelse(n_inj>1000,.9,.7))) +
    geom_text(aes(label=n_inj, hjust=ifelse(n_inj>1000,1.2,-0.5)), size=3, vjust=.3) +
    scale_y_continuous(expand = c(0,0), limits = c(0,3300)) +
    #scale_fill_viridis_c(option = "B") +
    labs(title = "",
       subtitle="\n",
       x="",
       y="\nTotal injuries 2010-2017\n",
       caption = "by: @amadeob12 | Source: https://saferparksdata.org/") +
    coord_flip() +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "grey25", color = "grey25"),
        panel.background = element_rect(fill = "grey28", color = "grey25"),
        plot.title = element_text(size=25, face = "bold", color="#F8CE49"),
        plot.subtitle = element_text(size=15, color="grey85", face = "bold"),
        plot.caption = element_text(size=9, color="grey60"),
        text = element_text(color="grey85"),
        axis.title.x = element_text(color = "grey75", size=11),
        axis.title.y = element_text(color = "grey75", size=11),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color = "grey70", size=9, vjust = .5),
        #axis.line.x = element_line(color = "grey70", size=.7),
        axis.ticks.x = element_line(color = "grey35"),
        legend.position = "none") 
)

#devtools::install_github("thomasp85/patchwork") #elegir opción 3 (none)
library(patchwork)

plot_3b + plot_3.1 + plot_layout(ncol = 2, widths = c(5.5, .5))


library(cowplot)
plot_grid(plot_3b, plot_3.1, ncol = 2)


library(gridExtra)
(co_plot <- grid.arrange(plot_3b, plot_3.1, ncol = 2, widths = c(0.8, 0.22)))

ggsave(co_plot, file="coplot_park_3.png", width =17, height = 10, dpi = 600)
ggsave(co_plot, file="coplot_park_linkedin.png", width =14, height = 9, dpi = 500)

