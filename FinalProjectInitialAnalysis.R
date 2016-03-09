# Initial Setup ####
mm <- read.csv("https://dl.dropbox.com/s/a6r2v7co7h91pbg/finalProjectRawCSV.csv")
# , stringsAsFactors = FALSE

#Removes rowas with N/A in the datafram
mm <- head(mm,-5)

#Fixes the fact that hindfoot length comes in as a factor
mm$hindfoot.length <- as.numeric(as.character(mm$hindfoot.length))
mm <- mm[!is.na(mm$hindfoot.length),]

volumes = (mm$neurocranium.height * mm$neurocranium.length * mm$neurocranium.width)
mm$Volume <- volumes
mm$total.cubed.root <- mm$total.body.length^(1/3)

#1 Visualize ####
# Most popular specimens in the dataset
table(mm$Family.name)
# Muridae and Soricidae have the highest amounts of measurements. 
# Sciuridae and Vespertilionidae has the second highest amount.
# Some of these species are barely observed, so I'm going to remove
# them and consider the ones that have over 50 records.

topSpecies <-mm[mm$Family.name == "'VESPERTILIONIDAE'" | mm$Family.name == "'SORICIDAE'" | mm$Family.name == "'MURIDAE'"| mm$Family.name == "'SCIURIDAE'" | mm$Family.name == "'GEOMYIDAE'" , ]  
topSpecies$Family.name <- factor(topSpecies$Family.name)
table(topSpecies$Family.name)
mm <- topSpecies
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
jpeg("transformationAttempts1.jpg")
par(mfcol = c(1,1))
plot(Volume ~ total.body.length, data=mm)
dev.off()

# Plot neurocranium volume's relationship to body size.
jpeg("transformationAttempts2.jpg")
par(mfcol = c(1,1))
plot(Volume ~ sqrt(total.body.length), data=mm)
dev.off()

# Plot neurocranium volume's relationship to body size.
jpeg("transformationAttempts3.jpg")
par(mfcol = c(1,1))
plot(Volume ~ total.cubed.root, data=mm)
dev.off()

# Plot neurocranium volume's relationship to body size.
jpeg("transformationAttempts4.jpg")
par(mfcol = c(1,1))
plot(Volume ~ log(total.body.length), data=mm)
dev.off()

# We can see that there is a correlation between each value and total body length
plot(neurocranium.height ~ total.body.length, data=mm)
plot(neurocranium.length ~ total.body.length, data=mm)
plot(neurocranium.width ~ total.body.length, data=mm)
# Interestingly, you can see that there are clusters in the chart
# Could these be due to differences in species?


# Trying out NMDS scaling to check for general differences
library(vegan)
subset <- mm[ ,c("total.body.length", "tail.length", "hindfoot.length", "Volume")]
# subset <- mm[ ,c("total.body.length", "Volume")]
mm.mds <- metaMDS(subset) 
str(mm.mds)
mm$nmds1 <- mm.mds$points[,1]
mm$nmds2 <- mm.mds$points[,2]

jpeg("nmdsGraphs.jpg")
pairs(mm[, 22:23], col= rainbow(4)[mm$Population.designation], pch = 16)
# You can see that there seems to be some disctinct group, but that there
# is a mix of colors in each. These groups may indicate difference in species
# but it may also show that there is no different in neurocranial volume
# between members of the same species that lived in City vs. Rural areas.

fig <- ordiplot(mm.mds, type = "none")
text(fig, "sites", label = as.character(1:dim(mm)[1]), col="black", bg="white", cex=1.1)
text(fig, "sites", label = as.character(1:dim(mm)[1]), col=rainbow(4)[mm$Population.designation], bg="white", cex=1.1)
# The ordiplot makes confirms the above.
dev.off()

#Checking out Means and SDs for a few species####
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
muridaeSd <-aggregate(Volume ~ Population.designation, muridae, sd)
muridaeOut <- merge(muridaeH, muridaeL, by=c("Population.designation"))
muridaeOut <- merge(out, muridaeW, by=c("Population.designation"))
muridaeOut <- merge(muridaeOut, muridaeV, by=c("Population.designation"))
muridaeOut <- merge(muridaeOut, muridaeSd, by=c("Population.designation"))
muridaeOut

soricidae <- mm[mm$Family.name == "'SORICIDAE'", ]
soricidaeH <- aggregate(neurocranium.height ~ Population.designation, soricidae, mean)
soricidaeL <- aggregate(neurocranium.length ~ Population.designation, soricidae, mean)
soricidaeW <-aggregate(neurocranium.width ~ Population.designation, soricidae, mean)
soricidaeV <-aggregate(Volume ~ Population.designation, soricidae, mean)
soricidaeSd <-aggregate(Volume ~ Population.designation, soricidae, sd)
soricidaeOut <- merge(soricidaeH, soricidaeL, by=c("Population.designation"))
soricidaeOut <- merge(soricidaeOut, soricidaeW, by=c("Population.designation"))
soricidaeOut <- merge(soricidaeOut, soricidaeV, by=c("Population.designation"))
soricidaeOut <- merge(soricidaeOut, soricidaeSd, by=c("Population.designation"))
soricidaeOut

vesper <- mm[mm$Family.name == "'VESPERTILIONIDAE'", ]
vesperH <- aggregate(neurocranium.height ~ Population.designation, vesper, mean)
vesperL <- aggregate(neurocranium.length ~ Population.designation, vesper, mean)
vesperW <-aggregate(neurocranium.width ~ Population.designation, vesper, mean)
vesperV <-aggregate(Volume ~ Population.designation, vesper, mean)
vesperSd <-aggregate(Volume ~ Population.designation, vesper, sd)
vesperOut <- merge(vesperH, vesperL, by=c("Population.designation"))
vesperOut <- merge(vesperOut, vesperW, by=c("Population.designation"))
vesperOut <- merge(vesperOut, vesperV, by=c("Population.designation"))
vesperOut <- merge(vesperOut, vesperSd, by=c("Population.designation"))
vesperOut

sciuridae <- mm[mm$Family.name == "'SCIURIDAE'", ]
sciuridaeH <- aggregate(neurocranium.height ~ Population.designation, sciuridae, mean)
sciuridaeL <- aggregate(neurocranium.length ~ Population.designation, sciuridae, mean)
sciuridaeW <-aggregate(neurocranium.width ~ Population.designation, sciuridae, mean)
sciuridaeV <-aggregate(Volume ~ Population.designation, sciuridae, mean)
sciuridaeSd <-aggregate(Volume ~ Population.designation, sciuridae, sd)
sciuridaeOut <- merge(sciuridaeH, sciuridaeL, by=c("Population.designation"))
sciuridaeOut <- merge(sciuridaeOut, sciuridaeW, by=c("Population.designation"))
sciuridaeOut <- merge(sciuridaeOut, sciuridaeV, by=c("Population.designation"))
sciuridaeOut <- merge(sciuridaeOut, sciuridaeSd, by=c("Population.designation"))
sciuridaeOut

# 2 State Null & Alternate Hypotheses####
# Hypothesis - There is a difference in the neurocranial dimensions
# of animals of the same species that grow up in city or rural environments.

# 3 Choose Modeling Approach
# Since we have two Categorical Predictors, a continuous linear predictor and a 
# Continuous Response, we will try using a linear model.

# 4 Select Best Model
mAllInteract <- lm(Volume ~ Family.name*Population.designation*total.cubed.root, data = mm) 
mSplitInteract <- lm(Volume ~ Family.name*Population.designation + Population.designation*total.cubed.root + Family.name*total.cubed.root, data = mm) 

at1 <- anova(mAllInteract, mSplitInteract)
names(at1)[6] <- "P-value"
at1
# Not a significant difference

m1Interact <- lm(Volume ~ Family.name*Population.designation + Population.designation*total.cubed.root                                  , data = mm) 
m2Interact <- lm(Volume ~ Family.name*Population.designation +                                               Family.name*total.cubed.root, data = mm) 
m3Interact <- lm(Volume ~                                      Population.designation*total.cubed.root + Family.name*total.cubed.root, data = mm) 

anova(mSplitInteract, m1Interact) 
anova(mSplitInteract, m2Interact) #Not signifcant
anova(mSplitInteract, m3Interact) #Not signifcant 

mFamPopLengthInteract <- lm(Volume ~ Family.name:total.cubed.root + Family.name + total.cubed.root + Population.designation, data = mm)
mFamPopLength <- lm(Volume ~                                        Family.name + total.cubed.root + Population.designation, data = mm)

anova(mFamPopLengthInteract, mFamPopLength)
#mFamPopLengthInteract is our current best model.

mFamPopLengthInteract <- lm(Volume ~ Family.name:total.cubed.root + Family.name + total.cubed.root + Population.designation, data = mm)
mPopLengthInteract <- lm(Volume ~ Family.name:total.cubed.root +                  total.cubed.root + Population.designation, data = mm)
mFamPopInteract <-       lm(Volume ~ Family.name:total.cubed.root + Family.name                    + Population.designation, data = mm)
mFamLengthInteract <-    lm(Volume ~ Family.name:total.cubed.root + Family.name + total.cubed.root                         , data = mm)

anova(mFamPopLengthInteract, mPopLengthInteract, mFamPopInteract, mFamLengthInteract)
anova(mFamPopLengthInteract, mFamLengthInteract)

# After this anova, we see that mFamLengthInteract is the best model.
# This may indicate that population designation doesn't matter.

# 5 Check Model
par(mfcol = c(2,2))
plot(mFamLengthInteract) 
par(mfcol = c(1,1))
# By inspecting the plots, we see that the Residuals vs Fitted plot shows 
# that the mean of the Residals for each population is quite close to 0.
# The Normal Q-Q plot also shows that the data fits along the diagonal 
# somewhat well.

# 6 Report Parameters
confint(mFamLengthInteract)
coef(mFamLengthInteract)
summary(mFamLengthInteract)

# 7 Report Test Results
summary(mFamLengthInteract)






