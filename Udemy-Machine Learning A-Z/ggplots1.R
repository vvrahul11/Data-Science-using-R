http://www.sthda.com/english/articles/32-r-graphics-essentials/133-plot-one-variable-frequency-graph-density-distribution-and-more/?utm_content=buffer9285a&utm_medium=social&utm_source=facebook.com&utm_campaign=buffer
http://www.sthda.com/english/articles/32-r-graphics-essentials/129-visualizing-multivariate-categorical-data/?utm_content=buffer7d9d1&utm_medium=social&utm_source=facebook.com&utm_campaign=buffer
http://www.sthda.com/english/articles/17-tips-tricks/70-r-find-the-length-of-every-elements-in-a-list/?utm_content=buffereb8b2&utm_medium=social&utm_source=facebook.com&utm_campaign=buffer
http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/80-bar-plots-and-modern-alternatives/?utm_content=buffer65a99&utm_medium=social&utm_source=facebook.com&utm_campaign=buffer
http://www.sthda.com/english/articles/27-partitioning-clustering-essentials/89-clara-clustering-large-applications/?utm_content=buffer65228&utm_medium=social&utm_source=facebook.com&utm_campaign=buffer
http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/83-create-and-customize-multi-panel-ggplots-easy-guide-to-facet/?utm_content=buffer00607&utm_medium=social&utm_source=facebook.com&utm_campaign=buffer
http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/?utm_content=buffer42d01&utm_medium=social&utm_source=facebook.com&utm_campaign=buffer
http://www.sthda.com/english/articles/27-partitioning-clustering-essentials/87-k-means-clustering-essentials/?utm_content=buffer6049c&utm_medium=social&utm_source=facebook.com&utm_campaign=buffer
http://blog.gradientmetrics.com/2017/09/06/america-is-on-the-move/
	









##### TCGA data analysis 
#http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/77-facilitating-exploratory-data-visualization-application-to-tcga-genomic-data/
install.packages("ggpubr")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
library(ggpubr)
# Load the bioconductor installer. 
source("https://bioconductor.org/biocLite.R")
# Install the main RTCGA package
biocLite("RTCGA")
# Install the clinical and mRNA gene expression data packages
biocLite("RTCGA.clinical")
biocLite("RTCGA.mRNA")

library(RTCGA)
infoTCGA()

library(RTCGA)
library(RTCGA.mRNA)
expr <- expressionsTCGA(BRCA.mRNA, OV.mRNA, LUSC.mRNA,
                        extract.cols = c("GATA3", "PTEN", "XBP1","ESR1", "MUC1", "NR2A1", "NR1I3"))
expr
nb_samples <- table(expr$dataset)
nb_samples

expr$dataset <- gsub(pattern = ".mRNA", replacement = "",  expr$dataset)

expr$bcr_patient_barcode <- paste0(expr$dataset, c(1:590, 1:561, 1:154))
expr

#expr <- read.delim("https://raw.githubusercontent.com/kassambara/data/master/expr_tcga.txt",
                   stringsAsFactors = FALSE)

library(ggpubr)
# GATA3
ggboxplot(expr, x = "dataset", y = "GATA3",
          title = "GATA3", ylab = "Expression",
          color = "dataset", palette = "jco")
# PTEN
ggboxplot(expr, x = "dataset", y = "PTEN",
          title = "PTEN", ylab = "Expression",
          color = "dataset", palette = "jco")

# Create a  list of plots
p <- ggboxplot(expr, x = "dataset", 
               y = c("GATA3", "PTEN", "XBP1"),
               title = c("GATA3", "PTEN", "XBP1"),
               ylab = "Expression", 
               color = "dataset", palette = "jco")

expr <- expressionsTCGA(BRCA.mRNA, OV.mRNA, LUSC.mRNA,
                        extract.cols = c("NR2E3", "HNF4A", "NR1I3", "ESR1", "BRCA1"))
print("NR2E3 Vs ESR1")
cor.test(expr[1:590,3], expr[1:590,6])
print("HNF4A Vs ESR1")
cor.test(expr[1:590,4], expr[1:590,6])
print("NR1I3 Vs ESR1")
cor.test(expr[1:590,5], expr[1:590,6])

print("NR2E3 Vs BRCA1")
cor.test(expr[1:590,3], expr[1:590,7])
print("HNF4A Vs BRCA1")
cor.test(expr[1:590,4], expr[1:590,7])
print("NR1I3 Vs BRCA1")
cor.test(expr[1:590,5], expr[1:590,7])


ggboxplot(expr, x = "dataset", y = "HNF4A",
          title = "HNF4A", ylab = "Expression",
          color = "dataset", palette = "jco")

ggboxplot(expr, x = "dataset", y = "NR1I3",
          title = "NR1I3", ylab = "Expression",
          color = "dataset", palette = "jco")

ggboxplot(expr, x = "dataset", y = "NR2E3",
          title = "NR2E3", ylab = "Expression",
          color = "dataset", palette = "jco")

p <- ggboxplot(expr, x = "dataset", 
               y = c("HNF4A", "NR1I3"),
               title = c("HNF4A", "NR1I3"),
               ylab = "Expression", 
               color = "dataset", palette = "jco")



# View GATA3
p$GATA3
# View PTEN
p$PTEN
# View XBP1
p$XBP1

my_comparisons <- list(c("BRCA", "OV"), c("OV", "LUSC"))
ggboxplot(expr, x = "dataset", y = "GATA3",
          title = "GATA3", ylab = "Expression",
          color = "dataset", palette = "jco")+
  stat_compare_means(comparisons = my_comparisons)
