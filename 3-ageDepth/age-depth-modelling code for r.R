
install.packages("Bchron")
require(Bchron)
setwd("/Users/oliviaburge/Documents/paleo-R-workshop/")
# setwd("//richardhadlee/user1/mckeownm/Documents/r Workshop") # make sure you set your working directory to the correct file

# # Calibrating a single date
# Ages1 = BchronCalibrate(ages = 11500, ageSds = 230, calCurves = "shcal13", ids ="Date-1")
#
# summary(Ages1)
# plot(Ages1)
#
# # note the calCurves = "shcal13". If you want to calibrate
# # for Northern hemisphere change this to "intcal13".
# # marine is "marine13"
# # data for dummy calibration of nornallus distributed ages is "normal"
#
#
# # Calibrate multiple ages with different calibration curves
# Ages2=BchronCalibrate(ages = c(3445, 7632, 11500), ageSds = c(50,110, 230), calCurves = c("shcal13", "shcal13", "marine13"))
#
# summary(Ages2)
# plot(Ages2)
#
#
# # Calibrate multiple ages with multiple calibration curves and including depth
# Ages3=BchronCalibrate(ages = c(3445, 11500), ageSds = c(50,230), positions = c(100,150), calCurves = c("shcal13", "marine13"))
# summary(Ages3)
# plot(Ages3)

Ewe<-read.csv("3-ageDepth/Ewe.csv")  # your .csv needs to be set up in a particular way. See ewe.csv example

extractDate = 1950 - as.numeric(format(Sys.time(), "%Y")) # This will give you a value of -68 (1950 -(-68) = 2018)
surface = (extractDate = 1950 - as.numeric(format(Sys.time(), "%Y"))+21) # This will give you a value of -47, which is 1997, the year when the core was taken.

#Now we need to input this infomation into our Ewe.csv file.
#Please see file Ewe.csv (already changed for you).
#Note the - 47.

Eweout = Bchronology(ages = Ewe$ages,
                     ageSds = Ewe$ageSds,
                     calCurves = Ewe$calCurves,
                     positions = Ewe$position,
                     positionThicknesses = Ewe$thickness,
                     extractDate = surface,
                     ids = Ewe$id,
                     predictPositions = seq(0,500,by=5))
# you can change the values here. The 0 represents 0 cm, i.e.
# the top of the core and the 500 is 500 cm, the by=5 tells you
# to extract calibrated ages at 2.5%, 10%, 50%, 90% and 97.5%
# every 5 cm, you can change this to by=1 if you want every cm.
plot(Eweout)
summary(Eweout) # will extract the information


Eweout2 = Bchronology(ages = Ewe$ages,
                     ageSds = Ewe$ageSds,
                     calCurves = Ewe$calCurves,
                     positions = Ewe$position,
                     positionThicknesses = Ewe$thickness,
                     # extractDate = surface,
                     ids = Ewe$id,
                     predictPositions = seq(0,500,by=5))
all.equal(summary(Eweout2), summary(Eweout))

tiff("Ewe.tiff")  # this will allow you to extract the plot into your working directory as a .tiff. If you want a jpeg just change tiff for jpeg.
plot(Eweout, dateHeight = 30,
     withPositions = FALSE,
     pause = FALSE, borderCol = Null, fillCol = "gray",
     withHDR = TRUE, hdrCol = "darkgray")
dev.off()
