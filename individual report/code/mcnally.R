library(qgraph)
library(pcalg)
library(ggplot2)
library(dplyr)

source("code/R/CCD_fnc.R")
source("code/R/plot_fnc.R")
source("code/R/dsep_fnc.R")
source("code/R/searchAM_KP_fnc.R")
source("code/R/equivset_fnc.R")
source("code/R/data_generating_fnc.R")
source("code/R/eval_metric_fnc.R")

## empirical data example
mcnally <- read.csv("data/McNally.csv")

# separate dep / ocd symptoms
depression <- mcnally[,1:16]
ocd <- mcnally[,17:26]

##  graphical LASSO_ depression
cordep <- cor(depression)
glassoFitdep <- EBICglasso(cordep, n = nrow(depression))
qgraph(glassoFitdep, layout = "spring", theme="colorblind")

centrality_auto(glassoFitdep)
centralityPlot(glassoFitdep)


## run ccd on depression symptoms
ccd_mcnally_dep <- ccdKP(df=depression, dataType = "discrete", depth = -1)
mat_mcnally_dep <- CreateAdjMat(ccd_mcnally_dep, p = ncol(depression))
pag_mcnally_dep <- plotPAG(ccd_mcnally_dep, mat_mcnally_dep)

## equivalent class of DCGs
equiv_mcnallydep <- semiequiv_cdg2(ccd_mcnally_dep, mat_mcnally_dep)
save(equiv_mcnallydep, file="data/equiv_mcnallydep.RData")
load("data/equiv_mcnallydep.RData")

#layout(t(matrix(1:12, 3,4)))
lapply(equiv_mcnallydep, function(x) qgraph(t(x), layout="circle"))


## density comparison
GGMdensity(glassoFitdep)
DCGdensity(equiv_mcnallydep)

## degree centrality comparison
# we could put all together in one plot
GGMdegree(glassoFitdep) %>%
  mutate(title="Degree") %>%
  ggplot(aes(x = degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)

DCGdegree(equiv_mcnallydep) %>%
  mutate(title="Degree") %>%
  ggplot(aes(x = average_degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)







## run ccd on ocd symptoms
ccd_mcnally_ocd <- ccdKP(df=ocd, dataType = "discrete", depth = -1)
mat_mcnally_ocd <- CreateAdjMat(ccd_mcnally_ocd, p = ncol(ocd))
plotPAG(ccd_mcnally_ocd, mat_mcnally_ocd)





## use lavaan and get the edge weight from PAG?

library(lavaan)
cfa4p <- '
X2 ~ X1 + X3
X3 ~ X4 + X2'

fit <- cfa(cfa4p, data = data4p)
summary(fit)



cfa4p_high <- '
X2 ~ X1 + X3
X3 ~ X4 + X2
X4 ~ X1
'

fit <- cfa(cfa4p_high, data = data4p_high)
summary(fit)
B4_high # almost identical


cfa5p <- '
X1 ~ X2
X2 ~ X4
X3 ~ X2
X4 ~ X3
X5 ~ X4
'

fit <- cfa(cfa5p, data = data5p)
summary(fit)
B5 # almost identical


## idea1
# - have different structure of 4 nodes model and compute all equivalence set hopefully not too big --> then fit them in lavaan hopefully they are all identified. then we can use strength centrality instead of degree!

## idea2
#( - CCD propagation rule--> see if we can orient more those circle - circle with solid underline!!)

