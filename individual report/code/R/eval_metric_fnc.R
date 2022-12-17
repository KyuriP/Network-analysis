#' Compute density for GGM
#' @param qgraph.object the qgraph object
#'
#'
#' @return density value of GGM

GGMdensity <- function(qgraph.object){
  p <- qgraph.object[["graphAttributes"]][["Graph"]][["nNodes"]]
  density <- sum(round(abs(qgraph.object$Edgelist$weight), 2) > 0) / ((p * (p-1))/2)
  return(density)
}



#' Compute average density for equivalent class of DCGs (directed cyclic graph)
#' @param equivclass the list of equivalence class of DCG matrices
#'
#'
#' @return the average density of equivalent class of DCGs
DCGdensity <- function (equivclass){
  avg_density <- c()
  for(i in 1:length(equivclass)){
    p <- ncol(equivclass[[i]])
    # make symmetric
    x <- equivclass[[i]] + t(equivclass[[i]])
    x <- ifelse(x == 2, 1, x)
    edgenumber <- sum(x[lower.tri(x, diag = FALSE)])
    avg_density[i] <- edgenumber/ ((p * (p-1))/2)
  }
  return(mean(avg_density))
}


#' Compute density of true model (directed cyclic graph)
#' @param B regression matrix of true model
#'
#'
#' @return the average density of true DCG
truemoddensity <- function (B){
  p <- ncol(B)
  x <- ifelse(B==0, 0, 1)
  # make symmetric
  x <- x + t(x)
  x <- ifelse(x == 2, 1, x)
  edgenumber <- sum(x[lower.tri(x, diag = FALSE)])
  density <- edgenumber/ ((p * (p-1))/2)
  return(density)
}



#' Compute degree centrality for GGM
#' @param qgraph.object the qgraph object#'
#'
#' @return the dataframe of degree per node
GGMdegree <- function(qgraph.object){
  # get the existing edge indices
  ind <- round(abs(qgraph.object$Edgelist$weight), 2) >0
  # how many edges per node
  tb <- table(c(qgraph.object$Edgelist$from[ind], qgraph.object$Edgelist$to[ind]))
  # extract the node labels for table
  names(tb) <- qgraph.object$graphAttributes$Nodes$names[as.numeric(names(tb))]
  degree <- as.data.frame(tb)
  colnames(degree) <- c("node", "degree")
  return(degree)
}

#' Compute average density for equivalent class of DCGs (directed cyclic graph)
#' @param equivclass the list of equivalence class of DCG matrices
#'
#'
#' @return the dataframe of average degree of equivalent class of DCGs
DCGdegree <- function (equivclass){
  overall_degrees <- list()
  for(i in 1:length(equivclass)){
    outdegree <- colSums(equivclass[[i]])
    indegree <- rowSums(equivclass[[i]])
    overalldegree <- outdegree + indegree
    overall_degrees[[i]] <- overalldegree
  }
  avg_deg <- do.call(rbind, overall_degrees)
  avg_degree <- as.data.frame(apply(avg_deg, 2, mean))
  avg_degree$node <- rownames(avg_degree)
  colnames(avg_degree) <- c("average_degree", "node")
  rownames(avg_degree) <- NULL
  return(avg_degree[,c("node","average_degree")])
}

