#=============================================================================#
# CDW DB Network Plot - 2D and 3D                                             #
# Author: Richard Evans (Richard.Evans8@va.gov)                               #
# Development Start Date: 06/15/2018                                          #
#                                                                             #
# Network plots depicting relationships between Database tables and views in: #
# Server: vhacdwrb03.vha.med.va.gov                                           #
# DB: ORD_Sears_20180316D                                                     #
# There are 2 sources of meta-data, that which I have created (CDW), and that #
# which can be downloaded from CDWWork.Meta.DWViewForeignKey                  #
# Both are incomplete to some extent                                          #
#=============================================================================#

library(readxl)
library(tidyverse)
library(magrittr)

#==================== Relationship data ====================#

path <- "I:/Sears-CDA/3. Project Management/CDW_DB/ORD_Sears_201803016D.xlsx"

###### Data I created from the DB using the metadata views
cdw <- read_xlsx(path, sheet = "Relationships")

table(cdw$Entity1) # largest number of connections/edges: Surg_SurgeryINTRA
length(unique(cdw$Entity1)) # 112 views/tables in the DB

length(unique(cdw$Entity2)) # 140 nodes

length(unique(cdw$Link)) # 344 unique edges
length(cdw$Link) # 767 edges in total

###### VINCI data
metaVINCI <- read_xlsx(path, sheet = "metaVINCI")

table(metaVINCI$Entity1)
# largest number of edges: SStaff_SFeeServiceProvided
# tied with Fee_FeeServiceProvided

length(unique(metaVINCI$Entity1))
# 118 views/tables in the DB

length(unique(metaVINCI$Entity2))
# 137 nodes

length(unique(metaVINCI$Link))
# 352 unique edges

length(metaVINCI$Link)
# 823 edges in total (same as dim(metaVINCI)[1])

#==================== Nodes ====================#

views <- read_xlsx(path, sheet = "Views")

nodes <- views %>%
  mutate(
    ViewName = case_when(
      SchemaName == "CPRSOrder" ~ paste("CPRSOrder", ViewName, sep = "."),
      SchemaName == "TIU"       ~ paste("TIU", ViewName, sep = "."),
      SchemaName == "Dim"       ~ paste("Dim", ViewName, sep = "."),
      TRUE ~ ViewName
    ),
    logRowCount = log(RowCounts)
  ) %>%
  select(ViewName, logRowCount, Domain, PrimaryKey) %>%
  rename(node = ViewName)

edges <- cdw %>%
  rename(
    from = Entity1,
    to   = Entity2
  ) %>%
  filter(!is.na(from) & !is.na(to))

nodes <- data.frame(
    node = c(unique(edges$from), unique(edges$to)),
    stringsAsFactors = FALSE
  ) %>%
  distinct(node) %>%
  filter(!is.na(node)) %>%
  full_join(nodes, by = "node") %>%
  rename(id = node)

nodes <- nodes %>%
  anti_join(data.frame(
    id = setdiff(unique(nodes$id),
                 unique(c(edges$from, edges$to)))
  ),
  by = "id") %>%
  mutate(type = ifelse(Domain == "Dim", 1, 2))

edges <- edges %>%
  left_join(
    nodes %>%
      select(id, Domain),
    by = c("from" = "id")
  )

slice <- "Surg"
label <- "Domain"
directed <- TRUE
node_colors <- RColorBrewer::brewer.pal(9, "Set1")
node_colors2 <- c("#ffffff", node_colors[1])
node_size <- "logRowCount"
edge.arrow.size <- 0.2
edge.label <- NA

#============================== 2D Networks ==================================#

#' @title Create 2-Dimensional Network Plot of Relational DB views and tables
#' @param nodes
#' @param edges 
#' @param slice 
#' @param label 
#' @param directed
#' @param node_colors
#' @param node_size
DB2DNetwork <- function(nodes, 
                        edges, 
                        slice,
                        label,
                        directed = TRUE,
                        node_colors = NULL,
                        node_size = NULL,
                        type = "flat") {
  
  if(!require(igraph, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages("igraph", quiet = TRUE)
    print("Installing package: igraph")
  }
  
  network <- graph_from_data_frame(d = edges,
                                   vertices = nodes,
                                   directed = directed)
  
  # add edge and vertex attributes
  ## label
  E(network)$label <- edges[[label]]
  ## color to nodes based on type
  if (!is.null(node_colors)) {
    V(network)$color <- node_colors[V(network)$type]  
  }

  ## node size based on some attribute
  if (!is.null(node_size)) {
    V(network)$size <- nodes[[node_size]] 
  }
  
  # slice/subset
  eids <- which(grepl(slice, E(network)$label))
  subnetwork <- subgraph.edges(graph = network,
                               eids = eids,
                               delete.vertices = TRUE)
  
  if (type == "flat") {
    try(
      igraph::plot.igraph(subnetwork)
    )
  }
}

#============================== 3D Networks ==================================#

#' @title Create 3-Dimensional Network Plot of Relational DB views and tables
#' @param df dataframe with two columns, each containing nodes, where the relationship between column 1 and column 2 is from node1 to node2
#' @param slice selected CDW domain as a string. Internally passed to a grep call (case sensitive)
DB3DNetwork <- function(df, slice) {
  require(igraph)
  require(rgl)
  g <- df[grep(slice, df[[1]]), ]
  g <- graph_from_data_frame(g)
  coords <- layout_with_fr(g, dim = 3)
  rglplot(g, layout = coords, vertex.color = "green", edge.color = "red")
}

#============================= End of Document  ==============================#