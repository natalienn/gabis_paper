library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

data = read_csv("/Users/natal/OneDrive/Desktop/FrandLab/aambi_testing.csv")
data = dplyr::select(data, site_code, method, Index, Ecological_Category, Category_Color)%>%
  na.omit()
# categories 
data$Category <- cut(data$Index,
                     breaks = c(-Inf, 35, 49, 89, 120, Inf),
                     labels = c("Bad", "Fair", "Good", "Very Good", "Excellent"))

## A and Z are added just for highlighting aesthetics
site = c("A","SU-1","SU-2","SU-3","SU-4","PA-1","PA-2","PA-3","PA-4","TU-1","TU-2", "Z")
data$site_code = factor(data$site_code, levels = site)

ggplot(data, aes(x = site_code, y = Index, shape = method)) +
  geom_rect(aes(xmin = "A", xmax = "Z", ymin = -Inf, ymax = 35, fill = "Poor"), alpha = 0.05) +
  geom_rect(aes(xmin = "A", xmax = "Z", ymin = 35, ymax = 49, fill = "Fair"), alpha = 0.05) +
  geom_rect(aes(xmin = "A", xmax = "Z", ymin = 49, ymax = 89, fill = "Good"), alpha = 0.05) +
  geom_rect(aes(xmin = "A", xmax = "Z", ymin = 89, ymax = 120, fill = "Very good"), alpha = 0.05) +
  geom_rect(aes(xmin = "A", xmax = "Z", ymin = 120, ymax = Inf, fill = "Excellent"), alpha = 0.05) +
  geom_point(position = position_dodge(width = 0), size = 3, color = "darkgrey") +
  scale_shape_manual(values = c("meta" = 17, "morpho" = 15), name = "Methodology",
                     labels = c("Metabarcoding", "Morphological")) +
  scale_fill_manual(values = c("Excellent" = "skyblue", "Very good" = "forestgreen", "Good" = "gold2", "Fair" = "darkorange", "Poor" = "red3"),
                    name = "Category",
                    breaks = c("Excellent", "Very good", "Good", "Fair", "Poor")) +
  labs(x = "Site Code",
       y = "Index Score",
       fill = "Category",
       shape = "Methodology") +
  theme_minimal() +
  coord_cartesian(ylim = c(0, 200)) +
  theme(axis.text.x = element_text(size = 8, angle = 90, hjust = 1))
