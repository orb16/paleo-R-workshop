require(analogue)
require(rioja)

require(vegan)

require(tidyverse)

data(mite)
data(mite.env)

mOrd <- metaMDS(comm = mite,
                distance = "bray",
                k = 2,
                try = 100)

mOrd
stressplot(mOrd)

# plot with an ordihull
plot(mOrd) # sites and species
plot(mOrd, "species")
plot(mOrd, "sites")

ordihull(mOrd,
         groups = mite.env$Topo)



plotDataframe <- data.frame(mite.env,
                            scores(mOrd, "sites"))

unique(plotDataframe$Topo)

hummockPoints <- plotDataframe %>%
  filter(Topo == "Hummock")

blanketPoints <- plotDataframe %>%
  filter(Topo == "Blanket")

head(hummockPoints)
head(blanketPoints)

plot(mOrd, 
     type = "n") # start a blank plot
# points(mOrd, 
#        display = "sites",
#        col = "grey")
points(x = hummockPoints$NMDS1,
       y = hummockPoints$NMDS2,
       col = "red2") # just the hummockPoints

points(x = blanketPoints$NMDS1,
       y = blanketPoints$NMDS2,
       col = "forestgreen") # just the blanketPoints

ordiellipse(mOrd, 
            groups = mite.env$Topo, 
            kind = "sd", 
            show.groups = "Blanket", 
            lty = "dashed", # lty = linetype
            label = TRUE)


ordiellipse(mOrd, 
            groups = mite.env$Topo, 
            kind = "sd", 
            show.groups = "Hummock", 
            lty = "solid",
            label = TRUE)



getwd()
setwd("paleo-R-workshop/")
eweburnCounts <- read.csv(
  file = "1-folders-spreadsheets-organisingData/data/EWEBURN-9711PWKA.csv"
)

head(eweburnCounts)


eweburn6 <- eweburnCounts %>%
  filter(FullNames == "everything")%>%
  slice(1:6)
eweburn6


mites <- read.csv("2-paleo-data-analysis/data/vegan_mites.csv")


longMites <- mites %>%
  gather(key = "species",
         value = "counts", 
         Brachy:Trimalc2)
head()

unique(longMites$species)

shorterMites <- mites %>%
  gather(key = "species",
         value = "counts", 
         Brachy:Trimalc2) %>%
  filter(species %in% c("Protopl", "Brachy", "HPAV",
                        "Trimalc2"))
head(shorterMites)

ggplot(data = shorterMites, aes(x = counts)) +
  geom_histogram(binwidth = 10) +
  facet_wrap(~ species,
             scales = "free") + # scales = "free"
  theme_bw()


longMites %>%
  summarise(meanCount = mean(counts),
            maxCount = max(counts),
            minCount = min(counts))

countSummary <- shorterMites %>%
  group_by(species) %>%
  summarise(meanCount = mean(counts),
            minCount= min(counts),
            maxCount = max(counts))
countSummary

write.csv(x = countSummary,
          file = "2-paleo-data-analysis/out/tables/miteSummaryCounts.csv",
          row.names = FALSE)



# spreading

head(longMites)


ordinationdataframe <- longMites %>%
  spread(key = species, value = counts) %>%
  select(-c(plot))


newOrd <- metaMDS(comm = ordinationdataframe,
                  distance = "jaccard",
                  k = 4,
                  try = 50)

par(mfrow = c(2,2)) #par = graphical specs mfrow = n cols, nrows
plot(newOrd, choices = c(1,4))
plot(newOrd, choices = c(1,2))
plot(newOrd, choices = c(1,3))


list.files("3-ageDepth/")


ewe <- read.csv("3-ageDepth/Ewe.csv")
ewe2 <- read.csv("3-ageDepth/Ewe2.csv")

install.packages("Bchron")
library(Bchron)

Ages1 <- BchronCalibrate(ages = c(1000, 11500), ageSds = c(15, 230), 
                         calCurves = c("shcal13", "shcal13"),
                         ids = c("Date-1", "DateSECOND")
                         )

par(mfrow = c(1,2))
plot(Ages1)
summary(Ages1)


# Calibrate multiple ages with different calibration curves
ages2 = BchronCalibrate(ages=c(3445,11553,7456),ageSds=c(50,230,110),
                        calCurves=c('intcal13','intcal13','shcal13'))
summary(ages2)
plot(ages2)

# Calibrate multiple ages with multiple calibration curves and including depth
ages3 = BchronCalibrate(ages=c(3445,11553),ageSds=c(50,230),positions=c(100,150),
                        calCurves=c('intcal13','normal'))
summary(ages3)



plot(ages3, withPositions = TRUE)

summary(ages3)

secondBigOne <- Bchronology(ewe$ages, ageSds = ewe$ageSds,
                calCurves = ewe$calCurves,
                positions = ewe$position,
                positionThickness = rep(1, 7),
                predictPositions = seq(from = 0,to = 310, by = 5))
sOut <- summary(secondBigOne)
newOut <- data.frame(depth = sOut$Depth, medAge = sOut$`50%`)
head(newOut)
# 
# plot(x = newOut$med, y = newOut$depth)
# 
# tmp[tmp$depths > 90 & tmp$depths < 110,]
# 
# 
# tmpOut <- summary(secondBigOne)
# tmpOut
# 
# plot(secondBigOne)
# plot(tmpOut)
# 
# # remove the first line
# ewe <- ewe[-1,]
# 
# anotherB <- Bchronology(
#   ages = ewe$ages, 
#             ageSds = ewe$ageSds,
#             calCurves = ewe$calCurves,
#             positions = ewe$position,
#             extractDate = (1950 - 1995) # the 1995 is when you go tit
#             )
# 
# whyWeDoThis <- summary(anotherB)
# 
# 
# 
# newOut <- data.frame(depth = whyWeDoThis$Depth, medAge = whyWeDoThis$`50%`)
# 
# 
# # Predict for some new positions
# range(ewe$ages)
#   
# predictedAges <- predict(anotherB, newPositions = seq(from = 100, to = 8000, by = 10),
#                          newPositionThicknesses = rep(5, length(seq(from = 100, to = 8000, by = 10))))
# plot(predictAges)
# predictedAges
# str(predictedAges)
# 
# data.frame(Ages = predictedAges, Depths = seq(from = 100, to = 8000, by = 10))
# 
# 
# # Data from Glendalough
# data(Glendalough)
# 
# # Run in Bchronology - all but first age uses intcal13
# GlenOut = Bchronology(ages=Glendalough$ages,ageSds=Glendalough$ageSds,
#                       calCurves=Glendalough$calCurves,positions=Glendalough$position,
#                       positionThicknesses=Glendalough$thickness,ids=Glendalough$id,
#                       predictPositions=seq(0,1500,by=10))
# 
# # Summarise it a few different ways
# summary(GlenOut) # Default is for quantiles of ages at predictPosition values
# summary(GlenOut, type='convergence') # Check model convergence
# summary(GlenOut, type='outliers') # Look at outlier probabilities
# 
# # Predict for some new positions
# predictAges = predict(GlenOut, newPositions = c(150,725,1500), newPositionThicknesses=c(5,0,20))
# 
# # Plot the output
# plot(GlenOut,main="Glendalough",xlab='Age (cal years BP)',ylab='Depth (cm)',las=1)

