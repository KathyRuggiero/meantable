## ----set-options, echo=FALSE--------------------------------------------------------------------------------
options(width = 110)

## ---- echo=FALSE, message=FALSE-----------------------------------------------------------------------------
library(ruggi)

## -----------------------------------------------------------------------------------------------------------
data(fruitfly)

## ---- echo=FALSE, results='asis'----------------------------------------------------------------------------
knitr::kable(head(fruitfly))

## -----------------------------------------------------------------------------------------------------------
data(bugs)

## ---- echo=FALSE, results='asis'----------------------------------------------------------------------------
knitr::kable(head(bugs))

## -----------------------------------------------------------------------------------------------------------
newfruitfly <- toFactor(fruitfly, variables = c("ID", "Partners", "Type"))

## -----------------------------------------------------------------------------------------------------------
#replications(Thorax~Partners*Type, data=newfruitfly)

## ---- fig.height=4.5, fig.width=8, fig.align='center'-------------------------------------------------------
ggBoxplot(newfruitfly, y = "Longevity", x = "Partners", facets = c("Type", "."))
#groupSummaries(splitBlockData, y="logAUC", factors=c("Disease", "Organ"), FUN=meanse)

## ----echo=FALSE---------------------------------------------------------------------------------------------
newfruitfly <- transform(newfruitfly, Type = factor(Type, levels=c(9,0,1),
  labels=c("Control", "Newly pregnant", "Virgin")))

## ---- fig.height=4.5, fig.width=8, fig.align='center'-------------------------------------------------------
ggBoxplot(newfruitfly, y = "Longevity", x = "Partners", facets = c("Type", "."),
         xLabel = "Number of partners", yLabel = "Lifespan (days)")

## -----------------------------------------------------------------------------------------------------------
groupSummaries(newfruitfly, y="Longevity", factors=c("Type", "Partners"), 
               FUN=function(x) c(Median=median(x), IQR=IQR(x)))

## -----------------------------------------------------------------------------------------------------------
groupSummaries(newfruitfly, y="Longevity", factors=c("Type", "Partners"), 
               FUN=function(x) c(meanse(x), Var=var(x)))

## ---- include=FALSE-----------------------------------------------------------------------------------------
fivenum.df <- groupSummaries(newfruitfly, y="Longevity", factors=c("Type", "Partners"), fivenum)
names(fivenum.df)[-(1:2)] <- c("Min", "Hinge1", "Median", "Hinge2", "Max")
fivenum.df

## ---- echo=FALSE, results='asis'----------------------------------------------------------------------------
library(kableExtra)
repsTable <- knitr::kable(with(newfruitfly, table(Type, Partners)), "html")
add_header_above(repsTable, c("", "Partners" = 3))

## -----------------------------------------------------------------------------------------------------------
# create Control factor
newfruitfly$Control <- factor(ifelse(newfruitfly$Type=="Control", "Yes", "No"))
fruitfly.lm <- lm(Longevity ~ Control/(Partners*Type), data=newfruitfly)

## ---- echo=FALSE, results='asis'----------------------------------------------------------------------------
library(kableExtra)
knitr::kable(anova(fruitfly.lm), "html")

## -----------------------------------------------------------------------------------------------------------
fruitfly.pm <- predictmeans(fruitfly.lm, modelterm = "Control:Partners:Type", 
                            pairwise = TRUE, plot = FALSE)

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
fflypmClass.mat <- do.call('rbind', lapply(fruitfly.pm, class))
fflypmClass.df <- data.frame(Object=rownames(fflypmClass.mat), Class=fflypmClass.mat[,1])
#fflypmClass.df <- data.frame(rownames(fflypmClass.mat), fflypmClass.mat[,1])
#knitr::kable(fflypmClass.df, "html")
# pmTypeTable <- 
  knitr::kable(fflypmClass.df, row.names=FALSE)
#add_header_above(pmTypeTable, c("Type"=1, "Class"=1))

## -----------------------------------------------------------------------------------------------------------
fruitfly.pm$`Predicted Means`

## -----------------------------------------------------------------------------------------------------------
fruitfly.pm$`Standard Error of Means`

## -----------------------------------------------------------------------------------------------------------
fruitfly.mse <- means2df(fruitfly.pm, digits = 2)

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
knitr::kable(fruitfly.mse, align='cccrr', row.names = FALSE)

## -----------------------------------------------------------------------------------------------------------
fruitfly.pm[5]

## -----------------------------------------------------------------------------------------------------------
fruitfly.pm[6]

## -----------------------------------------------------------------------------------------------------------
fruitfly.tab <- makeSummaryTable(fruitfly.pm)

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
knitr::kable(makeSummaryTable(fruitfly.pm), row.names = FALSE)

## -----------------------------------------------------------------------------------------------------------
# select comparisons that do not include the control treatment
noControlComparisons <- fruitfly.tab$Comparison[5:10]

# find and keep only those rows with "interesting" comparisons
keepRows <- getFixedLevelComparisons(noControlComparisons, sepChar = ":")
fruitfly.tab2 <- rbind(fruitfly.tab[1:4, ], fruitfly.tab[5:10,][keepRows,])
fruitflyCompNames <- fruitfly.tab2$Comparison

# simplify comparison names
fruitfly.tab2$Comparison <- gsub("(Yes:0:)|No:", "", fruitfly.tab2$Comparison)

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
knitr::kable(fruitfly.tab2, row.names = FALSE)

## ---- fig.height=6, fig.width=8, fig.align='center'---------------------------------------------------------
confintplot(fruitfly.tab2, y="Difference", x = "Comparison",
            axisLabelSize = 16, tickMarkLabelSize = 12, xTickLabelAngle = 90)

## -----------------------------------------------------------------------------------------------------------
fruitflyCompNames

## -----------------------------------------------------------------------------------------------------------
disaggNames.df <- comparisonNames2df(fruitflyCompNames, split.at = ":",
                                     varNames = c("Control", "Partners", "Type"))

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
knitr::kable(disaggNames.df, row.names = FALSE)

## -----------------------------------------------------------------------------------------------------------
fruitfly.tab2 <- data.frame(Control=disaggNames.df[,1], fruitfly.tab2)
fruitfly.tab2 <- transform(fruitfly.tab2, 
                 Control = factor(Control, levels=c("Yes", "No"), labels=c("Celibate", "Active")))

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
knitr::kable(fruitfly.tab2, row.names = FALSE)

## ---- fig.height=6, fig.width=8, fig.align='center'---------------------------------------------------------
fruitfly.tab2$Comparison <- factor(fruitfly.tab2$Comparison)
confintplot(fruitfly.tab2, y="Difference", x = "Comparison", facets = c("Control", "."),
            axisLabelSize = 16, tickMarkLabelSize = 12, xTickLabelAngle = 90)

## ---- fig.height=6, fig.width=8, fig.align='center'---------------------------------------------------------
confintplot(fruitfly.tab2, y="Difference", x = "Comparison", facets = c("Control", "."),
            showSignif=TRUE, pvalVar="p", alpha = 0.05,
            axisLabelSize = 16, tickMarkLabelSize = 12, xTickLabelAngle = 90)

## -----------------------------------------------------------------------------------------------------------
fruitfly.list <- makeLSDlist(meansData = fruitfly.mse, y = "Mean", x = "Type", LSD = 8.293, 
                             edgeWidth = 0.05)

## ----echo=FALSE---------------------------------------------------------------------------------------------
fruitfly.list

## ---- fig.height=4.5, fig.width=8, fig.align='center'-------------------------------------------------------
meansLSDplot(lsdList =fruitfly.list, x = "Type", y = "Mean", 
             xLabel = "Type of partner", yLabel = "Predicted mean")

## ---- fig.height=4.5, fig.width=8, fig.align='center'-------------------------------------------------------
meansLSDplot(lsdList =fruitfly.list, x = "Type", y = "Mean",
             xLabel = "Type of partner", yLabel = "Predicted mean",
             colour = "Partners")

## ---- fig.height=4.5, fig.width=8, fig.align='center'-------------------------------------------------------
meansLSDplot(lsdList =fruitfly.list, x = "Type", y = "Mean",
             xLabel = "Type of partner", yLabel = "Predicted mean",
             shape = "Partners", pointSize = 3.5)

## ---- fig.height=4.5, fig.width=8, fig.align='center'-------------------------------------------------------
meansLSDplot(lsdList =fruitfly.list, x = "Type", y = "Mean",
             xLabel = "Type of partner", yLabel = "Predicted mean",
             colour = "Partners", shape = "Partners", pointSize = 3.5)

## ---- fig.height=4.5, fig.width=8, fig.align='center'-------------------------------------------------------
meansLSDplot(lsdList =fruitfly.list, x = "Type", y = "Mean",
             xLabel = "Type of partner", yLabel = "Predicted mean",
             shape = "Partners", pointSize = 3.5, lineType = "Partners")

## ---- fig.height=8, fig.width=8, echo=FALSE, include=FALSE--------------------------------------------------
fruitfly1 <- droplevels(newfruitfly[newfruitfly$Type!=9,])
pairs(fruitfly1[, c("Longevity", "Thorax", "Sleep")], oma=c(7,5,5,5),
      pch=21, cex=1.5,bg=c("blue", "green3")[fruitfly1$Type])
par(xpd=NA)
legend(x=0.29, y=0.05, legend=c("newly pregnant female", "virgin female"),
       ncol=2, pt.bg=c("blue", "green3"), pch=21, bty="n")
fruitfly1$logSleep <- log(fruitfly1$Sleep)
pairs(fruitfly1[, c("Longevity", "Thorax", "logSleep")], oma=c(7,5,5,5),
      pch=21, cex=1.5,bg=c("blue", "green3")[fruitfly1$Type])
par(xpd=NA)
legend(x=0.29, y=0.05, legend=c("newly pregnant female", "virgin female"),
       ncol=2, pt.bg=c("blue", "green3"), pch=21, bty="n")

## ---- fig.height=4.5, fig.width=8, echo=FALSE, fig.align='center'-------------------------------------------
ggplot(transform(newfruitfly, 
                 Partners = factor(Partners, labels=c("No partners", "1 partner", "8 partners"))), 
       aes(y=Longevity, x=Thorax, colour=Type)) +
  geom_point(size=2) +
  xlab("Thorax (mm)") +
  ylab("Lifespan (days)") +
  facet_grid(.~Partners) +
  #geom_smooth(method = "lm") + 
  geom_smooth(method="loess", span=0.95) +
  scale_colour_manual(values=c("blue", "red", "green3")) +
  theme_bw() +
  theme(strip.text.x = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12))

## -----------------------------------------------------------------------------------------------------------
groupCor(fruitfly, measure.vars=c("Longevity", "Thorax"), factors=c("Type"))

## -----------------------------------------------------------------------------------------------------------
groupCor(fruitfly, measure.vars=c("Longevity", "Thorax", "Sleep"), factors=c("Partners", "Type"))

## -----------------------------------------------------------------------------------------------------------
ffthorax.lm0 <- lm(Longevity ~ (Control/(Partners*Type))*Thorax, data=newfruitfly)
anova(ffthorax.lm0)

ffthorax.lm1 <- lm(Longevity ~ (Control/(Partners*Type)) + Thorax, data=newfruitfly)
anova(ffthorax.lm0, ffthorax.lm1)
anova(ffthorax.lm1)

ffthorax.pm <- predictmeans(ffthorax.lm1, modelterm = "Control:Partners:Type",
                            pairwise = TRUE, plot = FALSE)
ffthorax.tab <- makeSummaryTable(ffthorax.pm)
ffthorax.tab2 <- rbind(ffthorax.tab[1:4, ], ffthorax.tab[5:10,][keepRows,])
ffthorax.tab2$Comparison <- gsub("(Yes:0:)|No:", "", ffthorax.tab2$Comparison)
ffthorax.tab2 <- data.frame(Control=disaggNames.df[,1], ffthorax.tab2)
ffthorax.tab2 <- transform(ffthorax.tab2, 
                 Control = factor(Control, levels=c("Yes", "No"), labels=c("Celibate", "Active")))

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
knitr::kable(ffthorax.tab2, row.names = FALSE)

## -----------------------------------------------------------------------------------------------------------
ffthorax.tab2$Comparison <- factor(ffthorax.tab2$Comparison)
ffthorax.mse <- means2df(ffthorax.pm)
ffthorax.list <- makeLSDlist(meansData = ffthorax.mse, y = "Mean", x = "Type", 
                             LSD = mean(ffthorax.tab2$LSD), 
                             edgeWidth = 0.05)

## ---- fig.height=4.5, fig.width=8, echo=FALSE, fig.align='center'-------------------------------------------
meansLSDplot(lsdList =ffthorax.list, x = "Type", y = "Mean",
             xLabel = "Type of partner", yLabel = "Predicted mean",
             shape = "Partners", pointSize = 3.5, lineType = "Partners")

## -----------------------------------------------------------------------------------------------------------
data(bugs)

## ----results='asis'-----------------------------------------------------------------------------------------
knitr::kable(head(bugs), row.names = FALSE, align='r')

## ----fig.height=5, fig.width=8, fig.align='center'----------------------------------------------------------
bugs$logBaseline <- log10(bugs$Baseline + 1)
ggBoxplot(bugs, x="State", y="logBaseline", xLabel = "Disease state", 
          yLabel = "Baseline log10(Concentration)", facets=c("Bacteria", "."),
          axisLabelSize=18, tickMarkLabelSize = 12, xStripTextSize = 12,
          xTickLabelAngle = 90)

## ----echo=FALSE---------------------------------------------------------------------------------------------
# add weight column to d1 
# note: denominator n-1 is used by var(), giving an unbiased estimator
#       of the variance for i.i.d observations
var.mat <- with(bugs, tapply(logBaseline, list(State, Bacteria), var))
var.df <- melt(var.mat)
names(var.df) <- c("State", "Bacteria", "Var")
newbugs <- merge(bugs, var.df)
newbugs$Weight <- 1/newbugs$Var

# WLS two-way anova
baselineBugs.lm <- lm(logBaseline ~ State*Bacteria, data = newbugs, weights = Weight)
anova(baselineBugs.lm)

## ----echo=FALSE---------------------------------------------------------------------------------------------
baselineBugs.pm <- predictmeans(baselineBugs.lm, modelterm = "State:Bacteria", pairwise=TRUE, 
                                plot = FALSE)
bugs.m2df <- means2df(baselineBugs.pm, digit = 2)
bugs.m2df

#mlowBaselineWt.pm <- melt(lowBaselineWt.pm$"Predicted Means")
#names(mlowBaselineWt.pm) <- gsub("^(value.)", "", names(mlowBaselineWt.pm))
#loMeanSEM.df <- data.frame(mlowBaselineWt.pm, SEM=c(lowBaselineWt.pm$"Standard Error of Means"))
#with(loMeanSEM.df, loMeanSEM.df[order(Bacteria, State),])

## ---- fig.height=4.5, fig.width=8, echo=FALSE, fig.align='center'-------------------------------------------
ggplot(mapping = aes(x = State, y = Mean)) +
  geom_point(data = bugs.m2df) +
  facet_grid(.~Bacteria) +
  theme_bw()

## -----------------------------------------------------------------------------------------------------------
baselineBugs.tab <- makeSummaryTable(baselineBugs.pm)
dim(baselineBugs.tab)

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
knitr::kable(baselineBugs.tab[1:10,], row.names = FALSE)

## -----------------------------------------------------------------------------------------------------------
# min, max and average LSD from predictmeans()
c(baselineBugs.pm$LSD)

## -----------------------------------------------------------------------------------------------------------
keepRows <- getFixedLevelComparisons(baselineBugs.tab[,1], sepChar = ":")
baselineBugs.tab2 <- baselineBugs.tab[keepRows,]
dim(baselineBugs.tab2)

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
knitr::kable(baselineBugs.tab2[1:10,], row.names = FALSE)

## -----------------------------------------------------------------------------------------------------------
# min, max and average LSD from predictmeans()
with(baselineBugs.tab2, c(Max.LSD=max(LSD), Min.LSD=min(LSD), Mean=mean(LSD)))

## ---- fig.height=4.5, fig.width=8, echo=FALSE, fig.align='center'-------------------------------------------
names.df <- comparisonNames2df(baselineBugs.tab2$Comparison, split.at = ":", 
                               varNames = c("Disease", "Bacteria"))
baselineBugs.tab3 <- data.frame(names.df, baselineBugs.tab2[,-1])
dim(baselineBugs.tab3)
head(baselineBugs.tab3)
avgLSD.baseBugs3 <- mean(baselineBugs.tab3$LSD) # avg LSD = 0.298

baseBugs3a.list <- makeLSDlist(meansData = bugs.m2df, y = "Mean", x = "State",
                               LSD = avgLSD.baseBugs3, eqLevelFactors = "Bacteria",
                               lsdVar = "LSD_avg", edgeWidth = 0.1)
meansLSDplot(lsdList = baseBugs3a.list, x = "State", y = "Mean", facets = c("Bacteria","."),
             xLabel = "Disease state", yLabel = "log10(Cell count)",
             tickMarkLabelSize = 11, xTickLabelAngle = 90)


baseBugs3b.lsd <- getLSDsummary(baselineBugs.tab3, lsdVar="LSD", by = "Bacteria1")
baseBugs3b.lsd$LSD_avg <- avgLSD.baseBugs3
baseBugs3b.list <- makeLSDlist(meansData = bugs.m2df, y = "Mean", x = "State",
                               LSD = baseBugs3b.lsd, eqLevelFactors = "Bacteria",
                               lsdVar = "LSD_avg", edgeWidth = 0.1)
meansLSDplot(lsdList = baseBugs3b.list, x = "State", y = "Mean", facets = c("Bacteria","."),
             xLabel = "Disease state", yLabel = "log10(Cell count)",
             tickMarkLabelSize = 11, xTickLabelAngle = 90)


dim(baselineBugs.tab3)
baseBugs3c.lsd <- getLSDsummary(baselineBugs.tab3, lsdVar="LSD", by = "Bacteria1")
baseBugs3c.lsd
baseBugs3c.list <- makeLSDlist(meansData = bugs.m2df, y = "Mean", x = "State",
                              LSD = baseBugs3c.lsd, eqLevelFactors = "Bacteria",
                              lsdVar = "LSD_avg", edgeWidth = 0.1)
meansLSDplot(lsdList = baseBugs3c.list, x = "State", y = "Mean", facets = c("Bacteria","."),
             xLabel = "Disease state", yLabel = "log10(Cell count)",
             tickMarkLabelSize = 11, xTickLabelAngle = 90)

## -----------------------------------------------------------------------------------------------------------
with(baselineBugs.tab2, c(Max.LSD=max(LSD), Min.LSD=min(LSD), Mean=mean(LSD)))

## -----------------------------------------------------------------------------------------------------------
keepRows <- whichComparison(names.df, contrFactor = "Disease")
baselineBugs.tab4 <- baselineBugs.tab3[keepRows,]
dim(baselineBugs.tab4)

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
knitr::kable(baselineBugs.tab4, row.names = FALSE)

## ---- fig.height=4.5, fig.width=8, echo=FALSE, fig.align='center'-------------------------------------------
baseBugs4.lsd <- getLSDsummary(baselineBugs.tab4, lsdVar="LSD", by = "Bacteria1")
baseBugs4.lsd
baseBugs4.list <- makeLSDlist(meansData = bugs.m2df, y = "Mean", x = "State",
                              LSD = baseBugs4.lsd, eqLevelFactors = "Bacteria",
                              lsdVar = "LSD_avg", edgeWidth = 0.1)
meansLSDplot(lsdList = baseBugs4.list, x = "State", y = "Mean", facets = c("Bacteria","."),
             xLabel = "Disease state", yLabel = "log10(Cell count)",
             tickMarkLabelSize = 11, xTickLabelAngle = 90)

## ----echo=FALSE, results='asis'-----------------------------------------------------------------------------
knitr::kable(baselineBugs.tab4, row.names = FALSE)

## -----------------------------------------------------------------------------------------------------------
baselineBugs.tab5 <- baselineBugs.tab4[,-4][,c(2,1,3:10)]
baselineBugs.tab6 <- with(baselineBugs.tab5, 
                          data.frame(Bacteria=Bacteria1, 
                                     Comparison=paste0(Disease1,"-",Disease2),
                                     baselineBugs.tab5[,-(1:3)]))
baselineBugs.tab6 <- with(baselineBugs.tab6, baselineBugs.tab6[order(Bacteria),])
str(baselineBugs.tab6)

## ---- fig.height=4.5, fig.width=8, echo=FALSE, fig.align='center'-------------------------------------------
confintplot(baselineBugs.tab6, y="Difference", x="Comparison", showSignif = TRUE, pvalVar = "p",
            alpha = 0.05, facets = c("Bacteria", "."), xTickLabelAngle=90)

## -----------------------------------------------------------------------------------------------------------
baselineBugs.tab5 <- baselineBugs.tab4[,-4][,c(2,1,3:10)]
baselineBugs.tab7 <- baselineBugs.tab5[baselineBugs.tab5$Disease1=="Control",]
dim(baselineBugs.tab7)

## ---- fig.height=4.5, fig.width=8, echo=FALSE, fig.align='center'-------------------------------------------
baseBugs7.lsd <- getLSDsummary(baselineBugs.tab7, lsdVar="LSD", by = "Bacteria1")
baseBugs7.lsd
baseBugs7.list <- makeLSDlist(meansData = bugs.m2df, y = "Mean", x = "State",
                              LSD = baseBugs7.lsd, eqLevelFactors = "Bacteria",
                              lsdVar = "LSD_avg", edgeWidth = 0.1)
meansLSDplot(lsdList = baseBugs7.list, x = "State", y = "Mean", facets = c("Bacteria","."),
             xLabel = "Disease state", yLabel = "log10(Cell count)",
             tickMarkLabelSize = 11, xTickLabelAngle = 90)

## ---- fig.height=4.5, fig.width=8, echo=FALSE, fig.align='center'-------------------------------------------
#bugs$clogBase <- bugs$logBaseline - mean(bugs$logBaseline)
# -- log-transform response
bugs$logCells <- log10(bugs$Cells + 1)

# fit mixed model
#lowBugs.lme <- lme(logCells ~ Bacteria*State*Time*clogBase, random = ~1|Rat, data = bugs)
lowBugs.lme <- lme(logCells ~ Bacteria*State*Time*logBaseline, random = ~1|Rat, data = bugs)
anova(lowBugs.lme)

# -- get predicted means at logBase = 4.38
lowBugs.pm <- predictmeans(lowBugs.lme, modelterm = "Bacteria:State:Time", covariate = 4.38,
                           pairwise=TRUE, plot = FALSE)
lowBugs.m2df <- means2df(lowBugs.pm)
lowBugs.m2df

# post-hoc tests summary table
lowBugs.tab <- makeSummaryTable(lowBugs.pm)
head(lowBugs.tab)
dim(lowBugs.tab)

dim(lowBugs.tab[lowBugs.tab$p<0.05,])

# keep only pairwise comparisons in which only the levels of one factor differ
bugs.keepRows <- getFixedLevelComparisons(lowBugs.tab$Comparison, sepChar = ":")
lowBugs.tab1 <- lowBugs.tab[bugs.keepRows,]
dim(lowBugs.tab1)
dim(lowBugs.tab1[lowBugs.tab1$p<0.05,])

# break up comparison names
compNames.df <- comparisonNames2df(lowBugs.tab1$Comparison, split.at = ":",
                                   varNames = c("Bacteria", "State", "Time") )
newlowBugs.tab1 <- data.frame(compNames.df, lowBugs.tab1[,-1])

# pairwise comparisons between 4 and 24h within each Bacteria x State combination
newlowBugsTime.keeprows <- whichComparison(compNames.df, contrFactor="Time")
newlowBugs.tab2 <- newlowBugs.tab1[newlowBugsTime.keeprows,]
dim(newlowBugs.tab2)

# -- compute min, max and average LSD by Bacterial strain and disease State
newlowBugs2.lsd <- getLSDsummary(data = newlowBugs.tab2, lsdVar = "LSD",
                                 by = c("Bacteria1", "State1"))

#' # -- create list needed to plot means and LSDs
newlowBugs2.list <- makeLSDlist(meansData = lowBugs.m2df, y = "Mean", x = "Time",
                                LSD = newlowBugs2.lsd, eqLevelFactors = c("Bacteria", "State"),
                                lsdVar = "LSD_avg", edgeWidth = 0.05)

# plot means with LSD bars, facetting by Bacteria and State
meansLSDplot(lsdList = newlowBugs2.list, x = "Time", y = "Mean", facets = c("Bacteria","State"),
             xLabel = "Time (hours)", yLabel = "Predicted mean",
             tickMarkLabelSize = 11, xTickLabelAngle = 90) +
             geom_line(data = newlowBugs2.list[[1]], size = 0.9)

# pairwise comparisons between 4 and 24h within each Bacteria x State combination
newlowBugsState.keeprows <- whichComparison(compNames.df, contrFactor="State")
newlowBugs.tab3 <- newlowBugs.tab1[newlowBugsState.keeprows,]
dim(newlowBugs.tab3)

# -- compute min, max and average LSD by Bacterial strain and Time
newlowBugs3a.lsd <- getLSDsummary(data = newlowBugs.tab3, lsdVar = "LSD", 
                                  by = c("Bacteria1", "Time1"))
newlowBugs3a.lsd

# -- compute min, max and average LSD by Bacterial strain, as not very different by Time
newlowBugs3b.lsd <- getLSDsummary(data = newlowBugs.tab3, lsdVar = "LSD", by = "Bacteria1")
newlowBugs3b.lsd

# average LSD over bacterial strains
newlowBugs3b.list <- makeLSDlist(meansData = lowBugs.m2df, y = "Mean", x = "Time",
                                 LSD = newlowBugs3b.lsd, eqLevelFactors = "Bacteria",
                                 lsdVar = "LSD_avg", edgeWidth = 0.05)

# plot means facetting only by bacterial strain, using different line types for disease states
meansLSDplot(lsdList = newlowBugs3b.list, x = "Time", y = "Mean", facets = c("Bacteria", "."),
             xLabel = "Time (hours)", yLabel = "log10(Cell count)", shape = "State",
             lineType = "State", tickMarkLabelSize = 11, xTickLabelAngle = 90,
             xStripTextSize = 10, legendKeyWidth = 1.2)

# pairwise comparisons between Bacterial strains within States
newlowBugsBacteria.keeprows <- whichComparison(compNames.df, contrFactor="Bacteria")
newlowBugs.tab4 <- newlowBugs.tab1[newlowBugsBacteria.keeprows,]
names(newlowBugs.tab4)
newlowBugs.tab4 <- transform(newlowBugs.tab4, 
                             State1 = factor(State1, levels=c("Control", "HS", "AP", "DM")))
dim(newlowBugs.tab4)
with(newlowBugs.tab4, newlowBugs.tab4[order(State1, Bacteria1),])

# -- compute min, max and average LSD by Bacterial strain and Time
newlowBugs4a.lsd <- getLSDsummary(data = newlowBugs.tab4, lsdVar = "LSD", 
                                 by = c("State1", "Time1"))
newlowBugs4a.lsd

# -- compute min, max and average LSD by Bacterial strain, as not very different by Time
newlowBugs4b.lsd <- getLSDsummary(data = newlowBugs.tab4, lsdVar = "LSD", by = "State1")
newlowBugs4b.lsd

# average LSD over bacterial strains
newlowBugs4.list <- makeLSDlist(meansData = lowBugs.m2df, y = "Mean", x = "Time",
                                LSD = newlowBugs4b.lsd, eqLevelFactors = "State",
                                lsdVar = "LSD_avg", edgeWidth = 0.05)
newlowBugs4.list

# plot means facetting only by bacterial strain, using different line types for disease states
meansLSDplot(lsdList = newlowBugs4.list, x = "Time", y = "Mean", facets = c("State", "."),
             xLabel = "Time (hours)", yLabel = "log10(Cell count)", shape = "Bacteria",
             lineType = "Bacteria", tickMarkLabelSize = 11, xTickLabelAngle = 90,
             xStripTextSize = 10, legendKeyWidth = 1.2)

newlowBugs.tab4a <- newlowBugs.tab4[, c(2,3,1,4,7:13)]
with(newlowBugs.tab4a, newlowBugs.tab4a[order(State1, Time1),])

## ----echo=FALSE, eval=FALSE, include=FALSE------------------------------------------------------------------
#  ggplot(mapping = aes(y=Mean, x=Time, group=Bacteria)) +
#    geom_point(data = lowBugs.m2df, aes(shape=Bacteria)) +
#    geom_line(data = lowBugs.m2df, aes(linetype=Bacteria)) +
#    facet_grid(.~State) +
#    geom_hline(yintercept=4.38, linetype="dashed", colour="red") +
#    theme_bw()
#  
#  
#  
#  # let's get predicted means at average value of baseline covariate
#  lowBugs.AvgBaseline.pm <- predictmeans(lowBugs.lme, modelterm = "Bacteria:State:Time",
#                                         pairwise=TRUE, plot = FALSE)
#  lowBugs.AvgBaseline.m2df <- means2df(lowBugs.AvgBaseline.pm)
#  lowBugs.AvgBaseline.m2df
#  
#  ggplot(mapping = aes(y=Mean, x=Time, group=Bacteria)) +
#    geom_point(data = lowBugs.AvgBaseline.m2df, aes(shape=Bacteria)) +
#    geom_line(data = lowBugs.AvgBaseline.m2df, aes(linetype=Bacteria)) +
#    facet_grid(.~State) +
#    geom_hline(yintercept=4.272, linetype="dashed", colour="red") +
#    theme_bw()
#  
#  
#  lowBugs.AvgBaseline.pm <- predictmeans(lowBugs.lme, modelterm = "Bacteria:State:Time",
#                                         covariate=4.45, pairwise=TRUE, plot = FALSE)
#  lowBugs.AvgBaseline.m2df <- means2df(lowBugs.AvgBaseline.pm)
#  ggplot(mapping = aes(y=Mean, x=Time, group=Bacteria)) +
#    geom_point(data = lowBugs.AvgBaseline.m2df, aes(shape=Bacteria)) +
#    geom_line(data = lowBugs.AvgBaseline.m2df, aes(linetype=Bacteria)) +
#    facet_grid(.~State) +
#    geom_hline(yintercept=4.45, linetype="dashed", colour="red") +
#    theme_bw()
#  
#  ## replicate students' analysis for low baseline time=4
#  ##
#  
#  bugslow4 <- subset(bugs[,1:6], subset = Time == 4)
#  bugs$Rat <- factor(bugs$Rat)
#  str(bugslow4)
#  bugslow4$Ratio <- with(bugslow4, Cells/Baseline)
#  bugslow4$logRatio <- log10(bugslow4$Ratio)
#  with(bugslow4, tapply(log10(Baseline), list(Bacteria, State), mean))
#  with(bugslow4, tapply(log10(Baseline), Bacteria, mean))
#  
#  bugslow24 <- subset(bugs[,1:6], subset = Time == 24)
#  bugslow24$Cells[bugslow24$Cells==0] <- 67
#  bugslow24$Ratio <- with(bugslow24, Cells/Baseline)
#  bugslow24$logRatio <- log10(bugslow24$Ratio)
#  with(bugslow24, tapply(logRatio, list(Bacteria, State), mean))
#  with(bugslow24, tapply(logRatio, Bacteria, mean))
#  
#  bugs4low.lme <- lme(logRatio ~ Bacteria*State, random = ~1|Rat,
#                      data = bugslow4)
#  anova(bugs4low.lme)
#  
#  summary(aov(logRatio ~ Bacteria*State + Error(Rat), data = bugslow4))
#  model.tables(aov(logRatio ~ Bacteria*State + Error(Rat), data = bugslow4))
#  summary(aov(Ratio ~ Bacteria*State + Error(Rat), data = bugslow4))
#  
#  # -- get predicted means at logBase = 4.38
#  bugs4low.pm <- predictmeans(bugs4low.lme, modelterm = "Bacteria:State",
#                              pairwise=TRUE, plot = FALSE)
#  bugs4low.m2df <- means2df(bugs4low.pm)
#  ggplot(mapping = aes(y=Mean, x=State, group=Bacteria)) +
#    geom_point(data = bugs4low.m2df, aes(shape=Bacteria)) +
#    geom_hline(yintercept=0, linetype="dashed", colour="red") +
#    theme_bw()
#  
#  
#  bugs4low.lme <- lme(logRatio ~ Bacteria*State, random = ~1|Rat, data = bugslow4)
#  anova(bugs4low.lme)
#  
#  # -- get predicted means at logBase = 4.38
#  bugs4low.pm <- predictmeans(bugs4low.lm, modelterm = "Bacteria:State",
#                              pairwise=TRUE, plot = FALSE)
#  bugs4low.m2df <- means2df(bugs4low.pm)
#  ggplot(mapping = aes(y=Mean, x=State, group=Bacteria)) +
#    geom_point(data = bugs4low.m2df, aes(shape=Bacteria)) +
#    geom_hline(yintercept=0, linetype="dashed", colour="red") +
#    theme_bw()

## -----------------------------------------------------------------------------------------------------------
#bugslsd.df <- getLSDsummary(baselineBugs.tab4, lsdVar = "LSD", by = "Bacteria1")

## -----------------------------------------------------------------------------------------------------------
#makeLSDlist(data = bugs.mse, y = "Mean", x = "State", LSD = )

## ---- fig.show='hold'---------------------------------------------------------------------------------------
plot(1:10)
plot(10:1)

## ---- echo=FALSE, results='asis'----------------------------------------------------------------------------
knitr::kable(head(mtcars, 10))

