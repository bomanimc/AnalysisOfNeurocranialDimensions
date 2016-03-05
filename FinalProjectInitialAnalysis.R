# Initial Setup ####
mm <- read.csv("/Users/Bomani/Dropbox/mcClendon_QMEC2016/McClendonFinalProject/finalProjectRawCSV.csv")
# , stringsAsFactors = FALSE

#Removes rowas with N/A in the datafram
mm <- head(mm,-5)

#Fixes the fact that hindfoot length comes in as a factor
mm$hindfoot.length <- as.numeric(as.character(mm$hindfoot.length))
mm <- mm[!is.na(mm$hindfoot.length),]

volumes = (mm$neurocranium.height * mm$neurocranium.length * mm$neurocranium.width)
mm$Volume <- volumes
plot(Volume ~ total.body.length, data=mm)

# I notice that there are a few abnormally large volumes
# These may be skewing the data, so I'm going to remove them.
mm <- mm[mm$total.body.length < 600, ]

#1 Visualize ####
# Most popular specimens in the dataset
table(mm$Family.name)
# Muridae and Soricidae have the highest amounts of measurements. 
# Sciuridae and Vespertilionidae has the second highest amount.
# Some of these species are barely observed, so I'm going to remove
# them and consider the ones that have over 50 records.

top4Species <-mm[mm$Family.name == "'VESPERTILIONIDAE'" | mm$Family.name == "'SORICIDAE'" | mm$Family.name == "'MURIDAE'"| mm$Family.name == "'SCIURIDAE'" , ]  
top4Species$Family.name <- factor(top4Species$Family.name)
table(top4Species$Family.name)
mm <- top4Species
# Now mm only contains the species with a decent amount of records.

# This shows us that there aren't many records for ag and non-ag
table(mm$Population.designation)
idx = c("City", "Rural")
majorAreas <- mm[mm$Population.designation %in% idx,] 
majorAreas$Population.designation  <- factor(majorAreas$Population.designation)
mm <- majorAreas
table(majorAreas$Population.designation)
# Here, I just removed all of the non-ag and ag population
# records that doen't have much data.


# Plot neurocranium volume's relationship to body size.
plot(Volume ~ total.body.length, data=mm)
# We can see that there is a correlation between each value and total body length
plot(neurocranium.height ~ total.body.length, data=mm)
plot(neurocranium.length ~ total.body.length, data=mm)
plot(neurocranium.width ~ total.body.length, data=mm)
# Interestingly, you can see that there are clusters in the chart
# Could these be due to differences in species?

# Trying out NMDS scaling to check for general differences
library(vegan)
subset <- mm[ ,c("total.body.length", "tail.length", "hindfoot.length", "Volume")]
mm.mds <- metaMDS(subset) 
str(mm.mds)
mm$nmds1 <- mm.mds$points[,1]
mm$nmds2 <- mm.mds$points[,2]
pairs(mm[, 22:23], col= rainbow(4)[mm$Population.designation], pch = 16)
# You can see that there seems to be some disctinct group, but that there
# is a mix of colors in each. These groups may indicate difference in species
# but it may also show that there is no different in neurocranial volume
# between members of the same species that lived in City vs. Rural areas.

fig <- ordiplot(mm.mds, type = "none")
text(fig, "sites", label = as.character(1:dim(mm)[1]), col="black", bg="white", cex=1.1)
text(fig, "sites", label = as.character(1:dim(mm)[1]), col=rainbow(4)[mm$Population.designation], bg="white", cex=1.1)
# The ordiplot makes confirms the above.


# # add species abbreviations to plot 
# text(fig, "species", labels=names(mm[, 22:23]), cex=1.2, font=3, col = "black")
# 
# ##calculate and plot significant "environmental" vectors 
# fit <- envfit(mm.mds, mm[, 3], perm = 1000)
# fit
# plot(fit, p.max = 0.05, col = "blue", cex=1.2) # why no arrows?
# 
# # ?ordihull 
# ordihull(mm.mds, mm$Family.name, display = "sites", draw = "polygon")
# 
# fig <- ordiplot(c.mds, type = "none")
# text(fig, "sites", label = as.character(1:200), col=rainbow(15)[crabs$col_index], bg="white", cex=1.1)
# 
# ordihull(c.mds, crabs$sp, display = "sites", draw = "polygon")

# I can see the populatations have distinct difference in dimensions
# between the species.
plot(neurocranium.height ~ Family.name, data = mm)
stripchart(neurocranium.height ~ Family.name, data = mm, vertical = TRUE, add = TRUE, 
           method = "jitter", pch = 19, jitter = 0.1, col = "gray")
plot(neurocranium.length ~ Family.name, data = mm)
stripchart(neurocranium.length ~ Family.name, data = mm, vertical = TRUE, add = TRUE, 
           method = "jitter", pch = 19, jitter = 0.1, col = "gray")
plot(neurocranium.width ~ Family.name, data = mm)
stripchart(neurocranium.width  ~ Family.name, data = mm, vertical = TRUE, add = TRUE, 
           method = "jitter", pch = 19, jitter = 0.1, col = "gray")

# I should subset each species and compare the City vs Rural dimensions 
# within each species.
muridae <- mm[mm$Family.name == "'MURIDAE'", ]
muridaeH <- aggregate(neurocranium.height ~ Population.designation, muridae, mean)
muridaeL <- aggregate(neurocranium.length ~ Population.designation, muridae, mean)
muridaeW <-aggregate(neurocranium.width ~ Population.designation, muridae, mean)
muridaeV <-aggregate(Volume ~ Population.designation, muridae, mean)
muridaeOut <- merge(muridaeH, muridaeL, by=c("Population.designation"))
muridaeOut <- merge(out, muridaeW, by=c("Population.designation"))
muridaeOut <- merge(muridaeOut, muridaeV, by=c("Population.designation"))
muridaeOut

soricidae <- mm[mm$Family.name == "'SORICIDAE'", ]
soricidaeH <- aggregate(neurocranium.height ~ Population.designation, soricidae, mean)
soricidaeL <- aggregate(neurocranium.length ~ Population.designation, soricidae, mean)
soricidaeW <-aggregate(neurocranium.width ~ Population.designation, soricidae, mean)
soricidaeV <-aggregate(Volume ~ Population.designation, soricidae, mean)
soricidaeOut <- merge(soricidaeH, soricidaeL, by=c("Population.designation"))
soricidaeOut <- merge(soricidaeOut, soricidaeW, by=c("Population.designation"))
soricidaeOut <- merge(soricidaeOut, soricidaeV, by=c("Population.designation"))
soricidaeOut

vesper <- mm[mm$Family.name == "'VESPERTILIONIDAE'", ]
vesperH <- aggregate(neurocranium.height ~ Population.designation, vesper, mean)
vesperL <- aggregate(neurocranium.length ~ Population.designation, vesper, mean)
vesperW <-aggregate(neurocranium.width ~ Population.designation, vesper, mean)
vesperV <-aggregate(Volume ~ Population.designation, vesper, mean)
vesperOut <- merge(vesperH, vesperL, by=c("Population.designation"))
vesperOut <- merge(vesperOut, vesperW, by=c("Population.designation"))
vesperOut <- merge(vesperOut, vesperV, by=c("Population.designation"))
vesperOut

sciuridae <- mm[mm$Family.name == "'SCIURIDAE'", ]
sciuridaeH <- aggregate(neurocranium.height ~ Population.designation, sciuridae, mean)
sciuridaeL <- aggregate(neurocranium.length ~ Population.designation, sciuridae, mean)
sciuridaeW <-aggregate(neurocranium.width ~ Population.designation, sciuridae, mean)
sciuridaeV <-aggregate(Volume ~ Population.designation, sciuridae, mean)
sciuridaeOut <- merge(sciuridaeH, sciuridaeL, by=c("Population.designation"))
sciuridaeOut <- merge(sciuridaeOut, sciuridaeW, by=c("Population.designation"))
sciuridaeOut <- merge(sciuridaeOut, sciuridaeV, by=c("Population.designation"))
sciuridaeOut

# aggregate(neurocranium.height ~ Population.designation, muridae, mean)
# aggregate(neurocranium.height ~ Population.designation, muridae, length)
# aggregate(neurocranium.height ~ Population.designation, muridae, sd)
# 
# aggregate(neurocranium.height ~ Population.designation, soricidae, mean)
# aggregate(neurocranium.height ~ Population.designation, soricidae, length)
# aggregate(neurocranium.height ~ Population.designation, soricidae, sd)

# 2 State Null & Alternate Hypotheses


# 3 Choose Modeling Approach
# Since we have a Categorical Predictor and a 
# Continuous Response, we will try using a linear model.
# alt.model <- lm((neurocranium.height * neurocranium.length * neurocranium.width) ~ Population.designation, data = muridae)
# null.model <- lm((neurocranium.height * neurocranium.length * neurocranium.width) ~ 1, data = muridae)
# anova(null.model, alt.model)

# 4 Select Best Model
mInteract <- lm(Volume ~ Population.designation*total.body.length, data = muridae) 
mNoInteract <- lm(Volume ~ Population.designation + total.body.length, data = muridae) 
mPop <- lm(Volume ~ Population.designation, data = muridae)
mBody <- lm(Volume ~ total.body.length, data = muridae)
mNull <- lm(Volume ~ 1, data = muridae) 

anova(mInteract, mNoInteract)
anova(mNoInteract, mPop)
anova(mNoInteract, mBody)
anova(mNoInteract, mNull)


# 5 Check Model
par(mfcol = c(2,2))
plot(mNoInteract) 
par(mfcol = c(1,1))

# By inspecting the plots, we see that the Residuals vs Fitted plot shows 
# that the mean of the Residals for each population is quite close to 0.
# The Normal Q-Q plot also shows that the data fits along the diagonal 
# quite well. The Residual vs Leverage plot shows that the population residuals
# don't have a major effect on the slope.





