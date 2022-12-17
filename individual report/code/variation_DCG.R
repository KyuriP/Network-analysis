library(ggplot2)
library(dplyr)

#' Compute density for equivalent class of DCGs (directed cyclic graph)
#' @param equivclass the list of equivalence class of DCG matrices
#'
#'
#' @return the vector of density of equivalent class of DCGs
DCGdensities <- function (equivclass){
  density <- c()
  for(i in 1:length(equivclass)){
    p <- ncol(equivclass[[i]])
    # make symmetric
    x <- equivclass[[i]] + t(equivclass[[i]])
    x <- ifelse(x == 2, 1, x)
    edgenumber <- sum(x[lower.tri(x, diag = FALSE)])
    density[i] <- edgenumber/ ((p * (p-1))/2)
  }
  return(list(class_size =length(equivclass), densities = density, avg_density = mean(density), variation = var(density)))
}



#' Compute degree for equivalent class of DCGs (directed cyclic graph)
#' @param equivclass the list of equivalence class of DCG matrices
#'
#'
#' @return the dataframe of average degree of equivalent class of DCGs
DCGdegrees <- function (equivclass){
  overall_degrees <- list()
  for(i in 1:length(equivclass)){
    outdegree <- colSums(equivclass[[i]])
    indegree <- rowSums(equivclass[[i]])
    overalldegree <- outdegree + indegree
    overall_degrees[[i]] <- overalldegree
  }
  overall_deg <- do.call(rbind, overall_degrees)
  deg_var <- as.data.frame(apply(overall_deg, 2, sd))
  colnames(deg_var) <- "sd"
  avg_degree <- as.data.frame(apply(overall_deg, 2, mean))
  avg_degree$node <- rownames(avg_degree)
  colnames(avg_degree) <- c("average_degree", "node")
  rownames(avg_degree) <- NULL
  return(data.frame(node = avg_degree$node, avg_degree= avg_degree$average_degree, deg_sd = deg_var$sd))
}


## ===================
## density variation
## ===================
load("data/equiv4p.RData")
load("data/equiv4p_high.RData")
load("data/equiv5p.RData")
load("data/equiv5p_high.RData")
load("data/equiv6p.RData")
load("data/equiv6p_high.RData")

## density for DCGs per model
denvar4p <-DCGdensities(equiv4p)
denvar4phigh <-DCGdensities(equiv4p_high)
denvar5p <-DCGdensities(equiv5p)
denvar5phigh <-DCGdensities(equiv5p_high)
denvar6p <-DCGdensities(equiv6p)
denvar6phigh <-DCGdensities(equiv6p_high)
modeldensities <- list(denvar4p, denvar4phigh, denvar5p, denvar5phigh, denvar6p, denvar6phigh)
names(modeldensities) <- c("(a) 4 nodes - sparse", "(b) 4 nodes - dense", "(c) 5 nodes - sparse", "(d) 5 nodes - dense", "(e) 6 nodes - sparse", "(f) 6 nodes - dense")

# compute the true model densities
truemodels <- list(B4, B4_high, B5, B5_high, B6, B6_high) %>%
  purrr::map(~truemoddensity(.)) %>% unlist()

# specify bin numbers
bins <- c(15, 15, 15, 15, 20, 20)
# plot the distributions of density per model
plotlist1 <- list()
for(i in 1:length(modeldensities)){
  N <- modeldensities[[i]]$class_size[1]
  #pval <- t.test(modeldensities[[i]]$densities, mu = truemodels[i])$p.val
  plotlist1[[i]] <- as.data.frame(modeldensities[[i]]) %>%
    mutate(trueden = truemodels[i]) %>%
    ggplot(aes(x=densities))+
    geom_histogram(color="gray", fill="lightblue", bins = bins[i]) +
    xlim(0.25, 1.01) +
    geom_vline(aes(xintercept = avg_density, color = "average density of DCGs"), lwd = 0.3, linetype=2) +
    geom_vline(aes(xintercept = trueden, color = "density of true model"),lwd = 0.3, linetype=2) +
    theme_classic() + labs(title = names(modeldensities)[i], subtitle = paste("Size of equivalence class = ", N),  x = "density")+
    scale_color_manual(name = "", values = c("density of true model" = "red", "average density of DCGs" = "darkblue"))+
    theme(plot.title = element_text(size = 10, face="bold"),
          plot.subtitle=element_text(size=10, face="italic"))

}

ggpubr::ggarrange(plotlist=plotlist1, nrow=3, ncol=2, common.legend = T, legend = "bottom")




# t-test (make a table)
#t.test(denvar4p$densities, mu = truemoddensity(B4)) # not defined
t.test(denvar4phigh$densities, mu = truemoddensity(B4_high))
t.test(denvar5p$densities, mu = truemoddensity(B5))
t.test(denvar5phigh$densities, mu = truemoddensity(B5_high))
t.test(denvar6p$densities, mu = truemoddensity(B6))
t.test(denvar6phigh$densities, mu = truemoddensity(B6_high))



## ===========================
## degree centrality variation
## ===========================

## degrees centrality for DCGs
degvar4p <- DCGdegrees(equiv4p) %>% mutate(se = deg_sd * qnorm(0.975) / length(equiv4p), name = "(a) 4 nodes - sparse")
degvar4phigh <- DCGdegrees(equiv4p_high) %>% mutate(se = deg_sd * qnorm(0.975) / length(equiv4p_high), name = "(b) 4 nodes - dense")
degvar5p <- DCGdegrees(equiv5p) %>% mutate(se = deg_sd * qnorm(0.975) / length(equiv5p), name = "(c) 5 nodes - sparse")
degvar5phigh <- DCGdegrees(equiv5p_high)%>% mutate(se = deg_sd * qnorm(0.975) / length(equiv5p_high), name = "(d) 5 nodes - dense")
degvar6p <- DCGdegrees(equiv6p)%>% mutate(se = deg_sd * qnorm(0.975) / length(equiv6p), name = "(e) 6 nodes - sparse")
degvar6phigh <- DCGdegrees(equiv6p_high)%>% mutate(se = deg_sd * qnorm(0.975) / length(equiv6p_high), name = "(f) 6 nodes - dense")
modeldegrees <- list(degvar4p, degvar4phigh, degvar5p, degvar5phigh, degvar6p, degvar6phigh)
#names(modeldegrees) <- c("4 nodes-low density", "4 nodes-high density", "5 nodes-low density", "5 nodes-high density", "6 nodes-low density", "6 nodes-high density")

plotlist2 <- modeldegrees %>%
  purrr::map(~
               ggplot(data =., aes(x=node, y=avg_degree)) +
               # we multiply se by 100 as they are very very small... practically 0
               geom_errorbar(aes(ymin=avg_degree-se*100, ymax=avg_degree+se*100), width=0.1, color = "darkblue") +
               geom_line(group=1, color = "darkblue") + geom_point(size=0.7) + theme_minimal() +
               ylim(0, 5) + labs(title = .$name,  y = "average degree") +
               theme(plot.title = element_text(size = 10, face="bold"))
  )
ggpubr::ggarrange(plotlist=plotlist2, nrow=3, ncol=2, common.legend = T, legend = "bottom")


## make table for the actual se values
