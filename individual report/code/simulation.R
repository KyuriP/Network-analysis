library(qgraph)
library(pcalg)
library(qgraph)
library(ggplot2)
library(dplyr)

source("code/R/CCD_fnc.R")
source("code/R/plot_fnc.R")
source("code/R/dsep_fnc.R")
source("code/R/searchAM_KP_fnc.R")
source("code/R/equivset_fnc.R")
source("code/R/data_generating_fnc.R")
source("code/R/eval_metric_fnc.R")



###################
## basic 4 nodes ##
###################
# specify B matrix
p = 4
B4 = matrix(c(0, 0, 0, 0,
              1, 0, 0.5, 0,
              0, 0.5, 0, 0.9,
              0, 0, 0, 0), p, p, byrow = T)

colnames(B4) <- c("X1", "X2", "X3", "X4")
# specify layout
layout4 = matrix(c(-1,1,
                   -1,0,
                   1,0,
                   1,1),4,2,byrow = T)
## True graph
true4p <- qgraph(t(B4), layout=layout4, labels = colnames(B4), theme="colorblind")


## Data generating
# equilibrium check
equilibrium_check(B4)
# generate data
data4p <- gen_dat(B4, N =1e6, seed = 1)

## GGM
ggm4p <- qgraph(cor(data4p), layout=layout4, theme="colorblind")

## run CCD algorithm
ccd_4p <- ccdKP(df=data4p, dataType = "continuous", alpha = 0.05)
mat4p <- CreateAdjMat(ccd_4p, 4)
## PAG
pag4p <- plotPAG(ccd_4p, mat4p)
## equivalent class of DCGs
#equiv4p <- semiequiv_cdg2(ccd_4p, mat4p)
#save(equiv4p, file="data/equiv4p.RData")
load("data/equiv4p.RData")

#lapply(equiv4p, function(x) qgraph(t(x), layout="circle"))


# ## density comparison
# truemoddensity(B4)
# GGMdensity(ggm4p)
# DCGdensity(equiv4p)
#
# ## degree centrality comparison
# # we could put all together in one plot
# GGMdegree(true4p) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# GGMdegree(ggm4p) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# DCGdegree(equiv4p) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = average_degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#



###########################
##  4 nodes high density ##
###########################
# specify B matrix
p = 4
B4_high = matrix(c(0, 0, 0, 0,
                   0.9, 0, 0.4, 0,
                   0, 0.5, 0, .5,
                   -0.8, 0, 0, 0), p, p, byrow = T)
colnames(B4_high) <- c("X1", "X2", "X3", "X4")

## True graph
true4p_high <- qgraph(t(B4_high), layout=layout4, labels = colnames(B4_high), theme="colorblind")

## Data generating
# equilibrium check
equilibrium_check(B4_high)
# generate data
data4p_high <- gen_dat(B4_high, N =1e6, seed = 1)

## GGM
ggm4p_high <- qgraph(t(cor(data4p_high)), layout=layout4, theme="colorblind")

## run CCD algorithm
ccd_4p_high <- ccdKP(df=data4p_high, dataType = "continuous", alpha = 0.05)
mat4p_high <- CreateAdjMat(ccd_4p_high, 4)
# PAG
pag4p <- plotPAG(ccd_4p_high, mat4p_high)
# equivalent class of DCGs
#equiv4p_high <- semiequiv_cdg2(ccd_4p_high, mat4p_high) # takes some time but it runs (1659 graphs)
#save(equiv4p_high, file="data/equiv4p_high.RData")
load("data/equiv4p_high.RData")
#lapply(equiv4p_high, function(x) qgraph(t(x), layout="circle"))
# qgraph(t(equiv4p_high[[50]])) # just an example
#
# ## density comparison
# truemoddensity(B4_high)
# GGMdensity(ggm4p_high)
# DCGdensity(equiv4p_high)
#
# ## degree centrality comparison
# # we could put all together in one plot
# GGMdegree(true4p_high) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# GGMdegree(ggm4p_high) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# DCGdegree(equiv4p_high) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = average_degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#

###################
## basic 5 nodes ##
###################
# specify B matrix
p = 5

B5 = matrix(c(0, 1, 0, 0, 0,
              0, 0, 0, 0.7, 0,
              0, 0.4, 0, 0, 0,
              0, 0, .5, 0, 0,
              0, 0, 0, -1.5, 0), p, p, byrow = T)
colnames(B5) <- c("X1", "X2", "X3", "X4", "X5")
# specify layout
layout5 = matrix(c(0,1,
                   0,0,
                   1,-1,
                   2,0,
                   2,1),5,2,byrow = T)
## True graph
true5p <- qgraph(t(B5), layout=layout5, labels = colnames(B5), theme="colorblind")

## Data generating
# equilibrium check
equilibrium_check(B5)
# generate data
data5p <- gen_dat(B5, N =1e6, seed = 123)

## GGM
ggm5p <- qgraph(cor(data5p), layout = layout5, theme="colorblind")

## run CCD algorithm
ccd_5p <- ccdKP(df=data5p, dataType = "continuous", alpha = 0.05)
mat5p <- CreateAdjMat(ccd_5p, 5)
## PAG
pag5p <- plotPAG(ccd_5p, mat5p)
## equivalent class of DCGs
#equiv5p <- semiequiv_cdg2(ccd_5p, mat5p)
#save(equiv5p, file="data/equiv5p.RData")
load("data/equiv5p.RData")

#
# #layout(t(matrix(1:12, 3,4)))
# lapply(equiv5p[1:10], function(x) qgraph(t(x), layout="circle"))
#
#
# ## density comparison
# truemoddensity(B5)
# GGMdensity(ggm5p)
# DCGdensity(equiv5p)
#
# ## degree centrality comparison
# # we could put all together in one plot
# GGMdegree(true5p) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# GGMdegree(ggm5p) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# DCGdegree(equiv5p) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = average_degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)


###########################
##  5 nodes high density ##
###########################
# specify B matrix
p = 5

B5_high = matrix(c(0, 0.9, 0, 0, 0.6,
              0, 0, 0, 0.7, 0,
              0, 0.9, 0, 0, 0,
              0, 0, 0.5, 0, 0,
              0, 0, 0, 1, 0), p, p, byrow = T)
colnames(B5_high) <- c("X1", "X2", "X3", "X4", "X5")
# specify layout
layout5 = matrix(c(0,1,
                   0,0,
                   1,-1,
                   2,0,
                   2,1),5,2,byrow = T)

## True graph
true5p_high <- qgraph(t(B5_high), layout=layout5, labels = colnames(B5_high), theme="colorblind")

## Data generating
# equilibrium check
equilibrium_check(B5_high)
# generate data
data5p_high <- gen_dat(B5_high, N =1e6, seed = 1)

## GGM
ggm5p_high <- qgraph(cor(data5p_high), layout = layout5, theme="colorblind")

## run CCD algorithm
ccd_5p_high <- ccdKP(df=data5p_high, dataType = "continuous", alpha = 0.05)
mat5p_high <- CreateAdjMat(ccd_5p_high, 5)
## PAG
pag5p_high <- plotPAG(ccd_5p_high, mat5p_high)
## equivalent class of DCGs
#equiv5p_high <- semiequiv_cdg2(ccd_5p_high, mat5p_high)
#save(equiv5p_high, file="data/equiv5p_high.RData")
load("data/equiv5p_high.RData")

#lapply(equiv5p_high, function(x) qgraph(t(x), layout="circle"))

#
# ## density comparison
# truemoddensity(B5_high)
# GGMdensity(ggm5p_high)
# DCGdensity(equiv5p_high)
#
# ## degree centrality comparison
# # we could put all together in one plot
# GGMdegree(true5p_high) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# GGMdegree(ggm5p_high) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# DCGdegree(equiv5p_high) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = average_degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#

###################
## basic 6 nodes ##
###################
# specify B matrix
p = 6
B6 = matrix(c(0, 0, 0, 0, 0, 0,
              0.3, 0, 0.4, 0, 0, 0,
              0, 0, 0, 0.9, 0, 0,
              0, 0, 0, 0, 0.4, 0,
              0, 0, 1, 0, 0, 0,
              1, 0, 0, 0, 0.5, 0), p, p, byrow = T)
colnames(B6) <- c("X1", "X2", "X3", "X4", "X5", "X6")
# specify layout
layout6 = matrix(c(1, 2,
                   0,1,
                   0,0,
                   1,-1,
                   2,0,
                   2,1),6,2,byrow = T)

## True graph
true6p <- qgraph(t(B6), layout=layout6, labels = colnames(B6), theme="colorblind")

## Data generating
# equilibrium check
equilibrium_check(B6)
# generate data
data6p <- gen_dat(B6, N =1e6, seed = 123)

## GGM
ggm6p <- qgraph(cor(data6p), layout = layout6, theme="colorblind")


## run CCD algorithm
ccd_6p <- ccdKP(df=data6p, dataType = "continuous", alpha = 0.05)
mat6p <- CreateAdjMat(ccd_6p, 6)
# PAG
pag6p <- plotPAG(ccd_6p, mat6p)
# equivalent class of DCGs
#equiv6p <- semiequiv_cdg2(ccd_6p, mat6p)
#save(equiv6p, file="data/equiv6p.RData")
load("data/equiv6p.RData")
#layout(t(matrix(1:12, 3,4)))
# lapply(equiv6p[1:10], function(x) qgraph(t(x), layout="circle"))
#
#
# ## density comparison
# truemoddensity(B6)
# GGMdensity(ggm6p)
# DCGdensity(equiv6p)
#
# ## degree centrality comparison
# # we could put all together in one plot
# GGMdegree(true6p) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# GGMdegree(ggm6p) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# DCGdegree(equiv6p) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = average_degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
#

###################
## high 6 nodes ##
###################
# specify B matrix
p = 6
B6_high = matrix(c(0, 0, 0, 0, 0, 0,
              0.7, 0, 0.4, 0, 0, 0.9,
              0, 0, 0, 0.9, 0, 0,
              0, 0, 0, 0, 0.4, 0,
              0, 0, 1, 0, 0, 0,
              1, 0, 0, 0, 0.5, 0), p, p, byrow = T)
# colnames for B matrix is necessary for running CCD
colnames(B6_high) <- c("X1", "X2", "X3", "X4", "X5", "X6")


true6p_high <- qgraph(t(B6_high), layout=layout6, labels = colnames(B6), theme="colorblind")

## Data generating
# equilibrium check
equilibrium_check(B6_high)
# generate data
data6p_high<- gen_dat(B6_high, N =1e6, seed = 123)

## GGM
ggm6p_high <- qgraph(cor(data6p_high), layout = layout6, theme="colorblind")

## run CCD algorithm
ccd_6p_high <- ccdKP(df=data6p_high, dataType = "continuous", alpha = 0.05)
mat6p_high <- CreateAdjMat(ccd_6p_high, 6)
## PAG
pag6p_high <- plotPAG(ccd_6p_high, mat6p_high)
## equivalent class of DCGs
#equiv6p_high <- semiequiv_cdg2(ccd_6p_high, mat6p_high)
#save(equiv6p_high, file="data/equiv6p_high.RData")
load("data/equiv6p_high.RData")

#layout(t(matrix(1:12, 3,4)))
#lapply(equiv6p_high, function(x) qgraph(t(x), layout="circle"))


# ## density comparison
# truemoddensity(B6_high)
# GGMdensity(ggm6p_high)
# DCGdensity(equiv6p_high)
#
# ## degree centrality comparison
# # we could put all together in one plot
# GGMdegree(true6p_high) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# GGMdegree(ggm6p_high) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
# DCGdegree(equiv6p_high) %>%
#   mutate(title="Degree") %>%
#   ggplot(aes(x = average_degree, y=node)) +
#   geom_path(aes(group=1)) + geom_point() + theme_bw() +
#   labs(x="", y="") + facet_grid(. ~title)
#
#
#
