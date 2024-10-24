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

library(neotoma2)
devil_test <- get_sites(siteid = 666)
summary(devil_test)

devils_samples <- readRDS("./data/devils_samples.rds")
unique(devils_samples$variablename)
library(analogue)
data(Pollen)
head(Pollen)
ncol(Pollen)-1

data(Climate)
head(Climate)



pol_df <- mod_pol_east %>% 
    pivot_longer(-ID2, names_to = "variablename")


library("fuzzyjoin")

test_names <- stringdist_left_join(devils_samples, pol_df, by = "variablename")

pol_names <- as_tibble(tolower(colnames(mod_pol_east[ ,-1])))
length(pol_names)
devil_names <- as_tibble(unique(tolower(devils_samples$variablename)))
length(devil_names)
test_names2 <- stringdist_left_join(devil_names, pol_names, by = "value", max_dist = 4, distance_col = "distance", method = "jw") %>% 
  group_by(value.x) %>%
  slice_min(order_by=distance, n=2)

test_names2
test_names2[test_names2$distance > 0.1, ]

test_names2 %>%
  filter(distance < 0.3)  %>%
  arrange(desc(distance))


test_names2 %>% 
  group_by(value.x) %>%
  top_n(1, desc(distance)) %>%
  ungroup()


data(ImbrieKipp)
data(SumSST)
data(V12.122)

## merge training set and core samples
dat <- join(ImbrieKipp, V12.122, verbose = TRUE)

## extract the merged data sets and convert to proportions
ImbrieKipp <- dat[[1]] / 100
ImbrieKippCore <- dat[[2]] / 100

ik.mat <- mat(ImbrieKipp, SumSST, method = "chord")
ik.mat

## model summary
summary(ik.mat)

## fitted values
fitted(ik.mat)

## model residuals
resid(ik.mat)

coreV12.mat <- predict(ik.mat, matrix(rnorm(110*30), nrow = 110, ncol = 30), k = 3)
coreV12.mat <- predict(ik.mat, V12.122, k = 3)
coreV12.mat

x <- devil_jant
n.analogues <- predave$predictions$model$k
preds <- predave$predictions$model$predicted[n.analogues, ]
errors <- predave$model$rmsep[n.analogues]

n.analogues <- x$predictions$bootstrap$k
preds <- x$predictions$bootstrap$predicted[,n.analogues]
if(sample.specific)
    errors <- x$predictions$sample.errors$rmsep[, n.analogues]
else
    errors <- x$bootstrap$rmsep[n.analogues]

plot(preds, devils_ages, type = 'l')

devils_samples %>%
    tidyr::pivot_wider(id_cols = c(age, depth),
                       names_from = variablename,
                       values_from = prop)

devils_samples %>% select(depth, variablename, pollencount, prop) %>% dplyr::filter(variablename == "Acer") %>% arrange(desc(depth))

data(swapdiat, swappH, rlgh, package = "analogue")
dat <- join(swapdiat, rlgh, verbose = TRUE)
head(dat)
swapdiat <- dat$swapdiat / 100
rlgh <- dat$rlgh / 100

cbind(devils_sqdist[ ,1], devils_ad)

mat  <- matrix(rnorm(16), 4, 4)
diag(mat) <- 0
mat[row(mat)!=col(mat)]
mat[row(mat) == col(mat) + 1]

test <- cbind(devils_ad, sq_dist = c(0, devils_sqdist[row(devils_sqdist) == col(devils_sqdist) + 1]))
plot(test[ ,"age"], test[ ,"sq_dist"],
     xlim = c(max(test[ ,"age"]), min(test[ ,"age"])))


plot(c(0, devils_sqdist[row(devils_sqdist) == col(devils_sqdist) + 1]), origin_top[ ,"sq_dist"])
plot(c(0, devils_sqdist[row(devils_sqdist) == col(devils_sqdist) + 1]), origin_top[ ,"sq_dist"], xlim = c(max(origin_top[ ,"age"]), min(origin_top[ ,"age"])))


library(analogue)
data(swapdiat, swappH, rlgh, package = "analogue")
dat <- join(swapdiat, rlgh, verbose = TRUE)
swapdiat <- dat$swapdiat / 100
rlgh <- dat$rlgh / 100
swap.mat <- mat(swapdiat, swappH, method = "SQchord")





mod_pol_east %>%
  rename_with(tolower) %>% # convert to lowercase for matching
  select(c(id2, names_matches_filter$value.y))


mod_pol_east_match <- mod_pol_east %>%
  rename_with(tolower) %>% 
  rename(all_of(deframe(names_matches_filter[ ,1:2])))

mod_pol_east_match[which(is.na(mod_pol_east_match))] <- 0
mod_pol_east_match_prop <- as.matrix(mod_pol_east_match[-1] / rowSums(mod_pol_east_match[-1]))
rowSums(mod_pol_east_match_prop)
rowSums(mod_pol_east[-1], na.rm = TRUE)
mod_pol_east[2465:2470, ]


rowSums(devils_prop)
rowSums(devils_wide[-1])



full_join(mod_pol_east_match, devils_wide[-1])