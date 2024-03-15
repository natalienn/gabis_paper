library(tidyverse)
library(dplyr)
library(HH)

data = read_csv("C:\\Users\\natal\\OneDrive\\Desktop\\FrandLab\\Gabi\\dataset_Ecuador_2021.csv")
colors = c("#482677FF","#33638DFF","#238A8DFF") 

####family#####
family_meta_data =
  dplyr::select(data, site_code, method, family) %>%
  filter(method == "metabarcoding") %>%
  na.omit()
fmetad = 
  group_by(family_meta_data, site_code)%>%
  summarize(metabarcoding = n())
f_meta_list = split(family_meta_data$family, family_meta_data$site_code)

family_morpho_data =
  dplyr::select(data, site_code, method, family) %>%
  filter(method == "morphological") %>%
  na.omit()
fmorphod =
  group_by(family_morpho_data, site_code)%>%
  summarize(morphological = n())
f_morpho_list = split(family_morpho_data$family, family_morpho_data$site_code)

both_f <- numeric(length(f_meta_list))
for (i in seq_along(f_meta_list)) {
  both_f[i] <- length(intersect(f_meta_list[[i]], f_morpho_list[[i]]))
}

f_meta_list <- lapply(f_meta_list, function(x) unique(x))
f_morpho_list <- lapply(f_morpho_list, function(x) unique(x))

unique_meta_f <- lapply(seq_along(f_meta_list), function(i) setdiff(f_meta_list[[i]], f_morpho_list[[i]]))
unique_morpho_f <- lapply(seq_along(f_morpho_list), function(i) setdiff(f_morpho_list[[i]], f_meta_list[[i]]))

lengths_meta_f <- lengths(unique_meta_f)
lengths_morpho_f <- lengths(unique_morpho_f)

site = c("SU-1","SU-2","SU-3","SU-4","PA-1","PA-2","PA-3","PA-4","TU-1","TU-2")

fam_data = data.frame(Site = site, morphological = lengths_morpho_f, Both = both_f, metabarcoding = lengths_meta_f)

fam_data$Site <- factor(fam_data$Site, levels = site)

## fam_data is the format we need for the likert (using counts)
###Site morphological Both metabarcoding
#1 SU-1            11    7             5
#2 SU-2             1    1             1
#3 SU-3             9    4             4

HH::likert(Site~.,fam_data, main = list("Method Usage for Family by Site"), col = colors)
