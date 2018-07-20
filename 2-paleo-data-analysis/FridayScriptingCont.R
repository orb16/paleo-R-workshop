# Friday workshop CONTINUED

# set the working directory
setwd("\\\\RichardHadlee/user1/BurgeO/Documents/paleo-R-workshop")

# load libraries 
require(analogue)
require(vegan)
require(rioja)
require(tidyverse)

# the main dataset for today

eweburnCounts <- read.csv(
  file = "1-folders-spreadsheets-organisingData/data/EWEBURN-9711PWKA.csv"
)



eweburnWide <- eweburnCounts %>% 
  filter(!Sums %in% c("W", "F")) %>%     # drop the species in the sums W and F
  select(FullNames, counts, Depths) %>%  # take only one species-specific column, plus the counts and depths
  spread(key = FullNames, value = counts) %>%          # spread from long to wide
  data.frame()
eweburnWide



eweVeg <- eweburnWide %>% 
  select(X.iAlnus : X.Poa..40um.not..iChionochloa)  # select just the veg cols
nrow(eweVeg)
ncol(eweburnWide) # number of cols in the earlier object
ncol(eweVeg)      # number of cols in eweVeg
names(eweVeg)



# run the ordination based on eweburn

ord <- metaMDS(comm = eweVeg, 
               distance = "jaccard",
               k = 2)  # k = 2 = nuumber of dimensions
stressplot(ord)
plot(ord)
ord

plot(ord, "sites")
text(ord, "species")

plottingDat <- data.frame(
  eweburnWide %>% select(Depths),   # select Depths from non-veg dataset
  scores(ord, "sites"))             # take the sites scores from th eordination object
head(plottingDat)


plottingDat <- plottingDat %>%
  mutate(DepthCategorical = 
           ifelse(Depths < 70, "Shallow", "Deep"))
#View(plottingDat)

ordPlot <- ggplot(plottingDat, 
                  aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(colour = DepthCategorical), 
             size = 2)  +
  coord_equal()  +# necessary for ordination plots
  theme_minimal() +
  theme(
    legend.position = "bottom"
  ) 

ordPlot2 <- ordPlot +
  scale_colour_manual("Depth categorical",
                      limits = c("Shallow", 'Deep'),
                      values = c("orange", 
                                 "forestgreen"))
ordPlot2




# get the data in for stratigraphic plotting


newEweburn <- read.csv("2-paleo-data-analysis/data/eweburnFull.csv")
eweburnDictionary <- read.csv("2-paleo-data-analysis/data/eweburnFullDictionary.csv")
eweburnSpecies <- newEweburn%>% select(-Depths)
head(eweburnSpecies)
head(eweburnDictionary)

colSums(eweburnSpecies)   # sum each column
rowSums(eweburnSpecies)   # sum each row

myThreshold <- quantile(colSums(eweburnSpecies), probs = .7)
myAbsoluteThreshold <- 30

Stratiplot(x = eweburnSpecies[colSums(eweburnSpecies) > myThreshold], 
           y = newEweburn$Depths)

Stratiplot(x = eweburnSpecies, 
           y = newEweburn$Depths)

Stratiplot(x = eweburnSpecies[colSums(eweburnSpecies) > myThreshold], 
           y = newEweburn$Depths,
           type = "poly", 
           col.poly = "royalblue",
           rev = TRUE,
           # strip = TRUE,
           sort = "wa", 
           ylab = "Depth")



tp <- strat.plot(d = eweburnSpecies[colSums(eweburnSpecies) > myThreshold], 
                 yvar = newEweburn$Depths,
                 plot.poly = TRUE, col.poly = "darkblue",
                 col.poly.line = NA, 
                 exag = TRUE, 
                 y.rev=TRUE,
                 wa.order = "topleft")


VD <- vegdist(eweburnSpecies, "jaccard") # creates a distance matrix
clust <- chclust(VD, method = "coniss")
bstick(clust) # hmm perhaps 3 or 5



tp2 <- strat.plot(d = eweburnSpecies %>% select_if(colSums(.) > myThreshold), 
                  yvar = newEweburn$Depths, plot.poly = TRUE, #col.poly = ourCols,
                  col.poly.line = "black", exag = TRUE, srt.xlabel = 45,
                  y.rev=TRUE,
                  clust = clust, 
                  wa.order = "topleft", cex.ylabel = 0.8, cex.xlabel = 0.8)
addClustZone(tp2, clust, 3,
             "black", lwd = 3, lty = "dashed")
