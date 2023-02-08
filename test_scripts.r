library(Bchron)
library(splines)
data(Sluggan)
print(Sluggan)
x <- Sluggan$ages
y <- Sluggan$position
age_range <- seq(from = range(Sluggan$ages)[1], to = range(Sluggan$ages)[2], length.out = 250)

poly_ages <- lm(y ~ poly(x, 20))
plot(x = Sluggan$ages, y = Sluggan$position, cex = 1.5, col = 'red')
lines(age_range, predict(poly_ages, data.frame(x = age_range)))


cubic_ages <- lm(y ~ bs(x, knots = c(1000, 6000, 12000)))
plot(x = Sluggan$ages, y = Sluggan$position, cex = 1.5, col = 'red')
lines(age_range, predict(cubic_ages, data.frame(x = age_range)))

SlugganOut = with(Sluggan, 
               Bchronology(ages=ages,
                           ageSds=ageSds, 
                           calCurves=calCurves,
                           positions=position, 
                           positionThicknesses=thickness,
                           ids=id, 
                           predictPositions=seq(0,1500,by=10)))

data(Glendalough)
print(Glendalough)

GlenOut = with(Glendalough, 
               Bchronology(ages=ages,
                           ageSds=ageSds, 
                           calCurves=calCurves,
                           positions=position, 
                           positionThicknesses=thickness,
                           ids=id, 
                           predictPositions=seq(0,1500,by=10)))


summary(GlenOut)

ages2 = BchronCalibrate(ages=c(3445,11553,7456), 
                        ageSds=c(50,230,110), 
                        calCurves=c('intcal20','intcal20','shcal20'))
summary(ages2)
plot(ages2)
median(ages2$Date1$ageGrid)

library(analogue)
data(abernethy)
head(abernethy)
ncol(abernethy)
aber_dat <- abernethy[ , 1:36]
aber_sqchord <- analogue::distance(aber_dat, method = "SQchord")
plot(aber_sqchord)
Stratiplot(abernethy$Depth ~ abernethy$Ulmus + abernethy$Betula)
