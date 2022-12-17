library(Rgraphviz)

#' Create adjacency matrix
#'
#' @param ccd.obj the resulting object of ccdKP function
#' @param p the number of nodes
#'
#' @details
#' "0": no edge; "1": circle; "2": arrow; "3": tail
#'
#' @return an adjacency matrix of PAG
CreateAdjMat <- function(ccd.obj, p){
  ccd.edges <- ccd.obj$edges
  mat <- matrix(0, p, p, dimnames = list(ccd.obj$nodes, ccd.obj$nodes))

  # for now, I assume that the names of nodes are number....
  for (i in 1:length(ccd.edges)){

    nodes <- unlist(stringr::str_split(ccd.edges[i], " "))
    row <- nodes[1]
    column <- nodes[3]
    row_edgetype <- substr(nodes[2], 1, 1)
    column_edgetype <- substr(nodes[2], 3, 3)

    mat[row, column] <- column_edgetype
    mat[column, row] <- row_edgetype
  }
  mat[mat=="o"] <- 1
  mat[mat==">"|mat=="<"] <- 2
  mat[mat=="-"] <- 3
  # convert the character matrix to numeric matrix
  class(mat) <- "numeric"

  return(mat)
}


#' Extract nodes
#'
#' @param triples the triples in PAG
#'
#' @return the middle node in the triples
#'
extractnode <- function(triples) {
  triplesets <- unlist(stringr::str_extract_all(triples,  "(?<=\\<).+?(?=\\>)"))
  triplesets <- stringr::str_split(triplesets, ",")
  middlenode <- stringr::str_trim(sapply(triplesets, function(x) x[[2]]))
  return(middlenode)
}
#'
#'
#' #' Extract dotted-underlined triplet nodes
#' #'
#' #' @param triples the triples in PAG
#' #'
#' #' @return the first,second,third node in the dotted-underlined triples
#' #'
#' extractdottednodes <- function(dottedtriples) {
#'   triplesets <- unlist(stringr::str_extract_all(dottedtriples,  "(?<=\\<).+?(?=\\>)"))
#'   triplesets <- stringr::str_split(triplesets, ",")
#'   firstnode <- stringr::str_trim(sapply(triplesets, function(x) x[[1]]))
#'   middlenode <- stringr::str_trim(sapply(triplesets, function(x) x[[2]]))
#'   lastnode <- stringr::str_trim(sapply(triplesets, function(x) x[[3]]))
#'   return(data.frame(fistnode = firstnode, middlenode= middlenode, lastnode=lastnode))
#' }


#' Extract triples
#'
#' @param triples the triples in PAG from ccdobj
#'
#' @return the first,middle,laste node in the triples
#'
extracttriples <- function(triples) {
  triplesets <- unlist(stringr::str_extract_all(triples,  "(?<=\\<).+?(?=\\>)"))
  triplesets <- stringr::str_split(triplesets, ",")
  firstnode <- stringr::str_trim(sapply(triplesets, function(x) x[[1]]))
  middlenode <- stringr::str_trim(sapply(triplesets, function(x) x[[2]]))
  lastnode <- stringr::str_trim(sapply(triplesets, function(x) x[[3]]))
  return(data.frame(fistnode = firstnode, middlenode= middlenode, lastnode=lastnode))
}


#' Plot PAG (partial ancestral graph)
#'
#' @param amat the adjacency matrix of PAG
#' @param ccd.obj the resulting object of ccdKP function
#'
#' @details
#' "0": no edge; "1": circle; "2": arrow; "3": tail
#'
#' @return a PAG graph of graphNEL class
#'
plotPAG <- function(ccd.obj, amat)
{
  # get the underline triples
  underline <- ccd.obj$graph$getUnderLines()$toString()
  # get the dotted-underline triples
  dottedunderline <- ccd.obj$graph$getDottedUnderlines()$toString()
  # get underline node
  underline_node <- extractnode(underline)
  # get  dotted-underline node
  dottedunderline_node <- extractnode(dottedunderline)

  g <- as(amat, "graphNEL")
  nn <- nodes(g)
  p <- numNodes(g)
  n.edges <- numEdges(g)
  ah.list <- at.list <- vector("list", n.edges)
  l.names <- character(n.edges)
  amat[amat == 1] <- "odot"
  amat[amat == 2] <- "vee"
  amat[amat == 3] <- "none"
  iE <- 0
  for (i in 1:(p-1)) {
    x <- nn[i]
    for (j in (i+1):p) {
      y <- nn[j]
      if (amat[x,y] != 0) {
        iE <- iE + 1
        ah.list[[iE]] <- amat[x,y]
        at.list[[iE]] <- amat[y,x]
        l.names[[iE]] <- paste0(x,"~",y)
      }
    }
  }
  names(ah.list) <- names(at.list) <- l.names

  graph <- layoutGraph(g) # layoutType="neato"
  fill <- rep("lightblue", length(underline_node))
  names(fill) <- underline_node
  nodeRenderInfo(graph) <- list(fill = fill)

  lty <- rep(2, length(dottedunderline_node))
  names(lty) <- dottedunderline_node
  nodeRenderInfo(graph) <- list(lty = lty)

  edgeRenderInfo(graph) <- list(arrowhead = ah.list, arrowtail = at.list)
  # global features
  graph.par(list(nodes=list(cex = 1)))
  pag <- Rgraphviz::renderGraph(graph)
  cat(paste("Dotted-underlined triples:", dottedunderline, "\nUnderlined triples:", underline, "\n"))
  return(pag)
}



# ## ==========================
# ## Examples
# ## ==========================
# mat4 <- CreateAdjMat(ccd_4p, 4)
# plotPAG(ccd_4p, mat4)
#
# mat6 <- CreateAdjMat(ccd_6p, 6)
# plotPAG(ccd_6p, mat6)
#
# mat_mcnally <- CreateAdjMat(ccd_mcnally, p = 26)
# plotPAG(ccd_mcnally, mat_mcnally)




## =======================================
## plotAG function (edited slightly)
## =======================================

plotAG_KP <- function(amat)
{
  g <- as(amat,"graphNEL")
  nn <- nodes(g)
  p <- numNodes(g)
  n.edges <- numEdges(g)
  ah.list <- at.list <- vector("list", n.edges)
  l.names <- character(n.edges)
  amat[amat == 1] <- "odot"
  amat[amat == 2] <- "vee"
  amat[amat == 3] <- "none"
  iE <- 0
  for (i in 1:(p-1)) {
    x <- nn[i]
    for (j in (i+1):p) {
      y <- nn[j]
      if (amat[x,y] != 0) {
        iE <- iE + 1
        ah.list[[iE]] <- amat[x,y]
        at.list[[iE]] <- amat[y,x]
        l.names[[iE]] <- paste0(x,"~",y)
      }
    }
  }
  names(ah.list) <- names(at.list) <- l.names

  edgeRenderInfo(g) <- list(arrowhead = ah.list, arrowtail = at.list)
  Rgraphviz::renderGraph(Rgraphviz::layoutGraph(g))
}

