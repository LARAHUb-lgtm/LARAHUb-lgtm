library(ggplot2)
library(tidyr)
library(readxl)
library(dplyr)

# Lade die Excel-Datei
data <- read_excel("/Users/larahuber/Downloads/R_lollipopdiagram.xlsx", sheet = "Tabelle1")

# Speichere den Namen der ersten Spalte
bioactive_components <- colnames(data)[1]

# Daten aufbereiten
long_data <- pivot_longer(data, cols = -all_of(bioactive_components), names_to = "Outcome", values_to = "Value")
long_data <- long_data %>% filter(Value != 4)  # Entferne Zeilen mit Wert 4
long_data <- long_data[!is.na(long_data$Value), ]  # Entferne fehlende Werte
long_data[[bioactive_components]] <- factor(long_data[[bioactive_components]], levels = unique(data[[bioactive_components]]))
long_data$Outcome <- factor(long_data$Outcome, levels = colnames(data)[-1])  # Setze die Reihenfolge für Outcome

# Erstelle das Lollipop-Diagramm mit gestrichelten Umrandungen für Werte 5, 6, 7
ggplot(long_data, aes(x = .data[[bioactive_components]], y = Outcome)) +
  
  # Lollipop-Linien
  geom_segment(aes(x = .data[[bioactive_components]], xend = .data[[bioactive_components]], y = Outcome, yend = 0),
               color = "grey", size = 1.0) +  # Dunklere und dickere Linien
  
  # Punkte für alle Werte
  geom_point(data = long_data %>% filter(Value %in% c(5, 6, 7)),
             aes(fill = factor(Value), size = factor(Value)),
             shape = 21, color = "black", stroke = 1,  # Gestreifte Umrandung für Werte 5, 6, 7
             linetype = "dashed") +  # Gestreifte Umrandung
             
  # Punkte für andere Werte ohne gestrichelte Umrandung
  geom_point(data = long_data %>% filter(!Value %in% c(5, 6, 7)),
             aes(fill = factor(Value), size = factor(Value)),
             shape = 21, color = "black", stroke = 1) +
  
  labs(title = "Results bioactive compounds and psychomotor development in studies",
       x = "Bioactive Compounds",   # x-Achsenbezeichnung
       y = "Outcomes") +
  
  theme_minimal() +                                # Minimalistisches Theme
  theme(
    panel.background = element_blank(),             # Entferne das Hintergrundpanel
    plot.background = element_blank(),              # Entferne den Hintergrund des Plots
    axis.title = element_text(color = "black"),    # Achsentitel-Farbe
    axis.text = element_text(color = "black"),      # Achsentext-Farbe
    axis.text.x = element_text(angle = 90, hjust = 1),  # X-Achsen-Beschriftungen drehen
    plot.title = element_text(hjust = 0.5, face = "bold")  # Titel zentrieren und fett machen
  ) +
  
  coord_fixed() +  # Dies sorgt dafür, dass die Zellen des Gitters quadratisch sind
  
  # Benutzerdefinierte Farben und Größen für die Werte 0, 1, 2 und 3
  scale_fill_manual(values = c("0" = "blue", "1" = "red", "2" = "green", "3" = "green"),
                    breaks = c("0", "1", "2", "3")) +
  
  # Benutzerdefinierte Größen
  scale_size_manual(values = c("0" = 5, "1" = 5, "2" = 5, "3" = 8),
                    breaks = c("0", "1", "2", "3")) +
  
  guides(fill = "none", size = "none")  # Entferne die Legende
