library(qgraph)

# ##### ===============================================
# ##### Example 1: 4 obs vars cycle with 2 vars and 1 LV
# ##### ===============================================
# # specify B matrix
# p = 5
# L4 = matrix(c(0, 0, 0, 0, 0,
#               0.6, 0, 0, 0, 0,
#               0, 1, 0, 0.5, 0,
#               0, 0, 0.5, 0, 0.9,
#               0.7, 0, 0, 0, 0), p, p, byrow = T)
#
# colnames(L4) <- c("L1", "X1", "X2", "X3", "X4")
# # specify layout
# Layout4 = matrix(c(0,2,
#                     -1,1,
#                    -1,0,
#                    1,0,
#                    1,1),5,2,byrow = T)
# ## True graph
# L4p <- qgraph(t(L4), layout=Layout4, labels = colnames(L4), theme="colorblind")
#
# ## Data generating
# # equilibrium check
# equilibrium_check(L4)
# # generate data
# latent4p <- gen_dat(L4, N =1e6, seed = 1)
# # observed variables only
# obs4p <- latent4p[,-1]
# ## GGM
# ggm4pL <- qgraph(cor(obs4p), layout=layout4, theme="colorblind")
#
# ## run CCD algorithm
# ccd_4pL <- ccdKP(df=obs4p, dataType = "continuous", alpha = 0.05)
# mat4pL <- CreateAdjMat(ccd_4pL, 4)
# ## PAG
# pag4pL <- plotPAG(ccd_4pL, mat4pL)
# ## equivalent class of DCGs

##### ===============================================
##### Example 2: 4 obs vars cycle with 2 vars and 1 LV
##### ===============================================
# specify B matrix
p = 5
L4_2 = matrix(c(0, 0, 0, 0, 0,
              0.6, 0, 0, 0, 0,
              0, 1, 0, 0, 0,
              0, 0, 0, 0, 0.9,
              0.5, 0, 0, 0.7, 0), p, p, byrow = T)

colnames(L4_2) <- c("L1", "X1", "X2", "X3", "X4")
# specify layout
Layout4 = matrix(c(0,2,
                   -1,1,
                   -1,0,
                   1,0,
                   1,1),5,2,byrow = T)
## True graph
L4p_2 <- qgraph(t(L4_2), layout=Layout4, labels = colnames(L4), theme="colorblind")

## Data generating
# equilibrium check
equilibrium_check(L4)
# generate data
latent4p_2 <- gen_dat(L4_2, N =1e6, seed = 1)
# observed variables only
obs4p_2 <- latent4p_2[,-1]
## GGM
ggm4pL_2 <- qgraph(cor(obs4p_2), layout=layout4, theme="colorblind")

## run CCD algorithm
ccd_4pL_2 <- ccdKP(df=obs4p_2, dataType = "continuous", alpha = 0.05)
mat4pL_2 <- CreateAdjMat(ccd_4pL_2, 4)
## PAG
pag4pL_2 <- plotPAG(ccd_4pL_2, mat4pL_2)
## equivalent class of DCGs
#equiv4pL2 <- semiequiv_cdg2(ccd_4pL_2, mat4pL_2)
save(equiv4pL2, file="data/equiv4pL2.RData")
load("data/equiv4pL2.RData")
#lapply(equiv4p_high, function(x) qgraph(t(x), layout="circle"))
qgraph(t(equiv4p_high[[50]])) # just an example

## density comparison
truemoddensity(L4_2)
GGMdensity(ggm4pL_2)
DCGdensity(equiv4pL2)

## degree centrality comparison
# we could put all together in one plot
GGMdegree(L4p_2) %>%
  mutate(title="Degree") %>%
  filter(node!="L1") %>%
  ggplot(aes(x = degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)

GGMdegree(ggm4pL_2) %>%
  mutate(title="Degree") %>%
  ggplot(aes(x = degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)

DCGdegree(equiv4pL2) %>%
  mutate(title="Degree") %>%
  ggplot(aes(x = average_degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)


##### ===============================================
##### Example 3: 4 obs vars cycle with 3 vars and 1 LV
##### ===============================================
# specify B matrix
p = 5
L4_3 = matrix(c(0, 0, 0, 0, 0,
                0.6, 0, 0, 0, 0,
                0.5, 0, 0, 1, 0,
                0, 0, 0, 0, 0.9,
                0, 0, 1.2, 0, 0), p, p, byrow = T)

colnames(L4_3) <- c("L1", "X1", "X2", "X3", "X4")
# specify layout
Layout5 = matrix(c(-1,0,
                   0,1,
                   0,-1,
                   2,-1,
                   1,-3),5,2,byrow = T)
## True graph
L4p_3 <- qgraph(t(L4_3), layout=Layout5, labels = colnames(L4_3), theme="colorblind")

## Data generating
# equilibrium check
equilibrium_check(L4_3)
# generate data
latent4p_3 <- gen_dat(L4_3, N =1e6, seed = 1)
# observed variables only
obs4p_3 <- latent4p_3[,-1]
## GGM
layout4_2 = matrix(c(0,1,
                   0,-1,
                   2,-1,
                   1,-3),4,2,byrow = T)
ggm4pL_3 <- qgraph(cor(obs4p_3), layout=layout4_2, theme="colorblind")

## run CCD algorithm
ccd_4pL_3 <- ccdKP(df=obs4p_3, dataType = "continuous", alpha = 0.05)
mat4pL_3 <- CreateAdjMat(ccd_4pL_3, 4)
## PAG
pag4pL_3 <- plotPAG(ccd_4pL_3, mat4pL_3)
## equivalent class of DCGs
#equiv4pL3 <- semiequiv_cdg2(ccd_4pL_3, mat4pL_3)
save(equiv4pL3, file="data/equiv4pL3.RData")
load("data/equiv4pL3.RData")
#lapply(equiv4p_high, function(x) qgraph(t(x), layout="circle"))
qgraph(t(equiv4pL3[[50]])) # just an example

## density comparison
truemoddensity(L4_3)
GGMdensity(ggm4pL_3)
DCGdensity(equiv4pL3)

## degree centrality comparison
# we could put all together in one plot
GGMdegree(L4p_3) %>%
  mutate(title="Degree") %>%
  filter(node!="L1") %>%
  ggplot(aes(x = degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)

GGMdegree(ggm4pL_3) %>%
  mutate(title="Degree") %>%
  ggplot(aes(x = degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)

DCGdegree(equiv4pL3) %>%
  mutate(title="Degree") %>%
  ggplot(aes(x = average_degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)



##### ===============================================
##### Example 4: 5 obs vars cycle with 3 vars and 1 LV
##### ===============================================
# specify B matrix
p = 6
L5_1 = matrix(c(0, 0, 0, 0, 0, 0,
                -1, 0, 0, 0, 0, 0,
                0, 0.8, 0, 0, 1, 0,
                0, 0, 1, 0, 0, 0,
                0, 0, 0, 0.8, 0, -1.3,
                0.6, 0, 0, 0, 0, 0), p, p, byrow = T)

colnames(L5_1) <- c("L1", "X1", "X2", "X3", "X4", "X5")
# specify layout
Layout6 = matrix(c(0,2,
                   -1,1,
                   -1,0,
                   0,-1,
                   1,0,
                   1,1),6,2,byrow = T)
## True graph
trueL5_1 <- qgraph(t(L5_1), layout=Layout6, labels = colnames(L5_1), theme="colorblind")

## Data generating
# equilibrium check
equilibrium_check(L5_1)
# generate data
latent5p_1 <- gen_dat(L5_1, N =1e6, seed = 1)
# observed variables only
obs5p_1<- latent5p_1[,-1]
## GGM
layout5_1 = matrix(c(0,1,
                     0,-1,
                     1,-2,
                     2,-1,
                     2,1),5,2,byrow = T)
ggm5pL_1 <- qgraph(cor(obs5p_1), layout=layout5_1, theme="colorblind")

## run CCD algorithm
ccd_5pL_1 <- ccdKP(df=obs5p_1, dataType = "continuous", alpha = 0.05)
mat5pL_1 <- CreateAdjMat(ccd_5pL_1, 5)
## PAG
pag5pL_1 <- plotPAG(ccd_5pL_1, mat5pL_1)
## equivalent class of DCGs
#equiv5pL1 <- semiequiv_cdg2(ccd_5pL_1, mat5pL_1)
save(equiv5pL1, file="data/equiv5pL1.RData")
load("data/equiv5pL1.RData")

qgraph(t(equiv5pL1[[50]])) # just an example


## density comparison
truemoddensity(L5_1)
GGMdensity(ggm5pL_1)
DCGdensity(equiv5pL1)

## degree centrality comparison
# we could put all together in one plot
GGMdegree(L5_1) %>%
  mutate(title="Degree") %>%
  filter(node!="L1") %>%
  ggplot(aes(x = degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)

GGMdegree(ggm5pL_1) %>%
  mutate(title="Degree") %>%
  ggplot(aes(x = degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)

DCGdegree(equiv5pL1) %>%
  mutate(title="Degree") %>%
  ggplot(aes(x = average_degree, y=node)) +
  geom_path(aes(group=1)) + geom_point() + theme_bw() +
  labs(x="", y="") + facet_grid(. ~title)
