### Visualization
#http://www.sthda.com/english/articles/32-r-graphics-essentials/129-visualizing-multivariate-categorical-data/?utm_content=buffer7d9d1&utm_medium=social&utm_source=facebook.com&utm_campaign=buffer
#http://www.sthda.com/english/wiki/amazing-interactive-3d-scatter-plots-r-software-and-data-visualization#at_pco=smlwn-1.0&at_si=5a1086c78b4d6c55&at_ab=per-2&at_pos=0&at_tot=1
library(ggplot2)
library(ggpubr)
theme_set(theme_pubr())
#Bar plot
#Demo data set: HairEyeColor (distribution of hair and eye color and sex in 592 statistics students)
data("HairEyeColor")
df <- as.data.frame(HairEyeColor)
head(df)

ggplot(df, aes(x = Hair, y = Freq))+
  geom_bar(
    aes(fill = Eye), stat = "identity", color = "white",
    position = position_dodge(0.9)
  )+
  facet_wrap(~Sex) + 
  fill_palette("jco")


#baloon plot
#Demo data sets: Housetasks (a contingency table containing the frequency of execution of 13 house tasks in the couple.)
housetasks <- read.delim(
  system.file("demo-data/housetasks.txt", package = "ggpubr"),
  row.names = 1
)
head(housetasks, 4)

ggballoonplot(housetasks, fill = "value")+
  scale_fill_viridis_c(option = "C")

#Visualize a grouped frequency table. Demo data set: HairEyeColor. Create a multi-panel plot by Sex
df <- as.data.frame(HairEyeColor)
ggballoonplot(df, x = "Hair", y = "Eye", size = "Freq",
              fill = "Freq", facet.by = "Sex",
              ggtheme = theme_bw()) +
  scale_fill_viridis_c(option = "C")

#Mosaic plot
library(vcd)
mosaic(HairEyeColor, shade = TRUE, legend = TRUE) 

#Correspondence analysis

#Correspondence analysis can be used to summarize and visualize the information contained in a large contingency table formed by two categorical variables.

#Required package: FactoMineR for the analysis and factoextra for the visualization
library(FactoMineR)
library(factoextra)
res.ca <- CA(housetasks, graph = FALSE)
fviz_ca_biplot(res.ca, repel = TRUE)

