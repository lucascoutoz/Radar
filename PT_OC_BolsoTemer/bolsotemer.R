library(tidyverse)
library(ggplot2)
library(ggradar)
library(ggtext)
library(showtext)
library(ragg)


PREPPS <- PREPPS_Latam_V2 %>%
  filter (presidentname == "Jair Bolsonaro" | presidentname == "Michel Temer") %>%
  filter (dimensionname == "Left-Right" |
          dimensionname == "Relationship USA" | 
          #dimensionname == "Vilify" | 
          #dimensionname == "Religion" |
          dimensionname == "Indig Peoples/Minrts") %>%
  select(presidentname, dimensionname, score_pos) %>%
  rename(nome = presidentname)

  
PREPPS2 <- PREPPS %>%
  filter (dimensionname == "Vilify" | 
          dimensionname == "Religion")

PREPPS2$score_pos <- (PREPPS2$score_pos-20)*-1

PREPPINHO <- rbind(PREPPS, PREPPS2)

PREPPINHO2 <- PREPPINHO %>%
  mutate(across(where(is.numeric), round, 0)) %>%
  mutate(dimensao = case_when(dimensionname == "Left-Right" ~ "Esquerda-Direita",
                              dimensionname == "Relationship USA" ~ "Pró-EUA",
                              dimensionname == "Vilify" ~ "Vilanização",
                              dimensionname == "Religion" ~ "Religião na Política",
                              dimensionname == "Indig Peoples/Minrts" ~ "Minorias")) %>%
  select(nome, dimensao, score_pos) %>%
  pivot_wider(id_cols = nome, names_from = dimensao,
              values_from = score_pos)

##fonts 
font_add(family="regular", "NotoSansJP-Regular.otf")
font_add(family="bold", "Roboto-Bold.ttf")
showtext_auto()

#theme
trialtheme <- theme(
  #Title, Subtitle, Caption
  plot.title=element_text(family="bold", hjust=0, vjust = 1, size=60, color="black"),
  plot.title.position = "plot",
  plot.subtitle = element_markdown(family="bold", size=30, hjust=0, color="black"),
  plot.caption=element_text(family="bold", size=30, color="black", hjust=1),
  plot.caption.position = "plot",
  #Panel and Background
  panel.border=element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.background = element_rect(fill = "#Ebebad"),
  plot.background = element_rect(fill = "#Ebebad"),
  plot.margin=ggplot2::margin(0.5, 0.5, 0.5, 0.5, "in"),
  #Axes
  axis.ticks.length=unit(0.15, "cm"),
  axis.ticks = element_blank(),
  axis.line = element_blank(),
  axis.title = element_blank(),
  axis.text.y = element_blank(),
  axis.text.x = element_blank(),
  axis.text = element_text(family="bold", size=30, color="black", hjust=1),
  #Legend
  legend.position = "top",
  legend.background = element_rect(fill="#Ebebad", color="#Ebebad"),
  legend.box.background = element_rect(fill="#Ebebad", color="#Ebebad"),
  legend.text = element_text(family="regular", color="black", size=30),
  legend.title = element_blank(),
  legend.key = element_rect(fill="#Ebebad"))



fk <- PREPPINHO2 %>%
  #mutate_at(vars(-nome), rescale) %>% 
  ggradar(grid.max = 20, grid.mid = 10, values.radar = c("1", "10", "20"),
          plot.legend = T, axis.label.size = 14,
          grid.label.size = 14, 
          group.colours = c("#4E6E9B", "#51BB6F"),
          font.radar = "regular") +
  labs(title = "Bolsonaro e Temer cara a cara",
       subtitle = "Diferença marcante em relação às minorias, 
       ao envolvimento da religião na política e à vilanização dos seus oponentes",
       x = "",
       y = "",
       caption = "Dados: Wiesehomeier et al. (2021) | @oc_ipolunb") +
  trialtheme


ggsave("bolsotemer.png",
       plot=fk,
       device = agg_png(width = 8, height = 8, units = "in", res = 300))
