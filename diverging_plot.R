library(tidyverse)
library(readxl)
library(dplyr)
library(HH)

data = read_excel("C:\\Users\\natal\\OneDrive\\Desktop\\FrandLab\\metabarcoding_dataset_11Sept.xlsx")
colors = c("#482677FF","#33638DFF","#238A8DFF") 

####FAMILY#####
family_meta_data =
  dplyr::select(data, SiteCode, Methodology, Family) %>%
  filter(Methodology == "Metabarcoding") %>%
  na.omit()
fmetad = 
  group_by(family_meta_data, SiteCode)%>%
  summarize(Metabarcoding = n())
f_meta_list = split(family_meta_data$Family, family_meta_data$SiteCode)

family_morpho_data =
  dplyr::select(data, SiteCode, Methodology, Family) %>%
  filter(Methodology == "Morphological") %>%
  na.omit()
fmorphod =
  group_by(family_morpho_data, SiteCode)%>%
  summarize(Morphological = n())
f_morpho_list = split(family_morpho_data$Family, family_morpho_data$SiteCode)

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

fam_data = data.frame(Site = site, Morphological = lengths_morpho_f, Both = both_f, Metabarcoding = lengths_meta_f)

fam_data$Site <- factor(fam_data$Site, levels = site)

## fam_data is the format we need for the likert (using counts)
###Site Morphological Both Metabarcoding
#1 SU-1            11    7             5
#2 SU-2             1    1             1
#3 SU-3             9    4             4

HH::likert(Site~.,fam_data, main = list("Methodology Usage for Family by Site"), col = colors)


####GENUS#####
genus_meta_data =
  dplyr::select(data, SiteCode, Methodology, Genus) %>%
  filter(Methodology == "Metabarcoding") %>%
  na.omit()
gmetad = 
  group_by(genus_meta_data, SiteCode)%>%
  summarize(Metabarcoding = n())
g_meta_list = split(genus_meta_data$Genus, genus_meta_data$SiteCode)

genus_morpho_data =
  dplyr::select(data, SiteCode, Methodology, Genus) %>%
  filter(Methodology == "Morphological") %>%
  na.omit()
gmorphod =
  group_by(genus_morpho_data, SiteCode)%>%
  summarize(Morphological = n())
g_morpho_list = split(genus_morpho_data$Genus, genus_morpho_data$SiteCode)

both_g <- numeric(length(g_meta_list))
for (i in seq_along(g_meta_list)) {
  both_g[i] <- length(intersect(g_meta_list[[i]], g_morpho_list[[i]]))
}

g_meta_list <- lapply(g_meta_list, function(x) unique(x))
g_morpho_list <- lapply(g_morpho_list, function(x) unique(x))

unique_meta_g <- lapply(seq_along(g_meta_list), function(i) setdiff(g_meta_list[[i]], g_morpho_list[[i]]))
unique_morpho_g <- lapply(seq_along(g_morpho_list), function(i) setdiff(g_morpho_list[[i]], g_meta_list[[i]]))

lengths_meta_g <- lengths(unique_meta_g)
lengths_morpho_g <- lengths(unique_morpho_g)

site = c("SU-1","SU-2","SU-3","SU-4","PA-1","PA-2","PA-3","PA-4","TU-1","TU-2")

gen_data = data.frame(Site = site, Morphological = lengths_morpho_g, Both = both_g, Metabarcoding = lengths_meta_g)

gen_data$Site <- factor(gen_data$Site, levels = site)

HH::likert(Site~.,gen_data, main = list("Methodology Usage for Genera by Site"), col = colors)