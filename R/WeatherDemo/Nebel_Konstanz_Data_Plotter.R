library(tidyverse)
load('KN_Recent_Nebel_Pro_Tag.RData')
recent = nebel_pro_tag
load('KN_Historisch_Nebel_Pro_Tag.RData')
hist = nebel_pro_tag
nebel_pro_tag = rbind(hist, recent)


nebel_pro_tag %>%
  mutate(Jahr = year(Datum)) %>%
  group_by(Jahr) %>%
  summarise(
    anteil_nebel = mean(anteil_nebel))  %>%
  ggplot(aes(x = Jahr, y = anteil_nebel)) +
  geom_line() +
  geom_point(col='blue') +
  labs(title = 'Anteil der Wettermeldungen mit Nebel (Tagsüber 9-16, Konstanz)',
       x = 'Jahr',
       y = 'Anteil nebeliger Wettermeldungen') 
ggsave('nebel_jahre.png', width = 12, height = 8, dpi = 300)

  
## Plotting the monthly data
nebel_pro_tag %>%
  filter(Datum >= '2020-01-01') %>%
  mutate(Monat = month(Datum, label=TRUE)) %>% #View() 
  mutate(Jahr = year(Datum)) %>%
  group_by(Monat, Jahr) %>%
  summarise(
    anteil_nebel = mean(anteil_nebel)) %>% 
  ggplot(aes(x = Monat, y = anteil_nebel, col = as.factor(Jahr))) +
  geom_line() +
  geom_point()  +
  labs(title = 'Anteil der Wettermeldungen mit Nebel in Konstanz (Nach 2000)',
       x = 'Monat',
       y = 'Anteil nebeliger Wettermeldungen') 




# Berechnung des mittleren Nebelanteils pro Monat und Jahr, gruppiert nach Jahrzehnt
p = nebel_pro_tag %>%
  #filter(Datum >= '2000-01-01') %>%
  mutate(
    Monat = month(Datum, label = TRUE),  # Monat extrahieren
    Jahr = year(Datum),                  # Jahr extrahieren
    Jahrzehnt = floor(Jahr / 10) * 10,    # Jahrzehnt bestimmen
    Jahr_im_Jahrzent = Jahr - Jahrzehnt  # Jahr im Jahrzehnt bestimmen
  ) %>%
  group_by(Jahr, Monat, Jahrzehnt,Jahr_im_Jahrzent) %>%
  summarise(anteil_nebel = mean(anteil_nebel, na.rm = TRUE)) %>%  # Mittlerer Anteil pro Monat und Jahr
  ggplot(aes(x = Monat, y = anteil_nebel)) +
  geom_boxplot() +
  geom_point(aes(col=as.factor(Jahr_im_Jahrzent)), size=0.5) +
  facet_wrap(~ Jahrzehnt) +  # Facetten nach Jahrzehnt
  labs(
    title = 'Wettermeldungen mit Nebel (Konstanz zwischen 9-16h)',
    x = 'Monat',
    y = 'Anteil pro Monat',
    labs(col = "Jahr") 
  ) + ylim(0, 1) + theme(legend.position = c(0.8,0.15), legend.direction = "vertical")
p
ggsave('nebel_jahrzehnte.png', p, width = 12, height = 8, dpi = 300)
  #No legend


## Plotting the monthly data
nebel_pro_tag %>%
  filter(Datum >= '2000-01-01') %>%
  mutate(Monat = month(Datum, label=TRUE)) %>% #View() 
  mutate(Jahr = year(Datum)) %>%
  group_by(Monat, Jahr) %>%
  summarise(
    anteil_nebel = mean(anteil_nebel)) %>% 
  ggplot(aes(x = Monat, y = anteil_nebel)) +
  geom_boxplot() +
  labs(title = 'Anteil der Wettermeldungen mit Nebel in Konstanz (Nach 2000)',
       x = 'Monat',
       y = 'Anteil Wettermeldungen mit Nebel') +
  ylim(0, 1) 

nebel_pro_tag %>%
  filter(Datum <= '2000-01-01') %>%
  mutate(Monat = month(Datum, label=TRUE)) %>% #View() 
  mutate(Jahr = year(Datum)) %>%
  group_by(Monat, Jahr) %>%
  summarise(
    anteil_nebel = mean(anteil_nebel)) %>% 
  ggplot(aes(x = Monat, y = anteil_nebel)) +
  geom_boxplot() +
  labs(title = 'Anteil der Wettermeldungen mit Nebel in Konstanz (1952-)',
       x = 'Monat',
       y = 'Anteil Wettermeldungen mit Nebel') +
  ylim(0, 1) 


library(plotly)
fig <- plot_ly(nebel_pro_tag, x = ~Datum, y = ~anteil_nebel, type = 'scatter', mode = 'lines',
               name = 'Anteil Nebel (%)')
# Layout-Optionen festlegen, damit Datum auf der x-Achse angezeigt wird
fig <- fig %>% layout(
  title = "Zeitreihe der Nebelanteile mit Zoom-Funktion",
  xaxis = list(
    title = "Datum",
    type = "date",            # Stellt sicher, dass die x-Achse als Datum behandelt wird
    tickformat = "%Y-%m-%d",  # Formatierung der Datumsticks (Jahr-Monat-Tag)
    rangeslider = list(visible = TRUE)  # Rangeslider für Zoom
  ),
  yaxis = list(title = "Anteil Nebel (%)"),
  height = 600
)

# Zeige die interaktive Grafik
fig
library(htmlwidgets)
saveWidget(fig, "nebel_zeitreihe.html")
