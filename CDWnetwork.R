#=============================================================================#
# CDW DB Network Plot - 2D and 3D                                             #
# Author: Richard Evans (Richard.Evans8@va.gov)                               #
# Development Start Date: 06/15/2018                                          #
#                                                                             #
# Network plots depicting relationships between Database tables and views in: #
# Server: vhacdwrb03.vha.med.va.gov                                           #
# DB: ORD_Sears_20180316D                                                     #
#=============================================================================#

library(readxl)

# Relationship data
path <- "I:/Sears-CDA/3. Project Management/CDW_DB/ORD_Sears_201803016D.xlsx"
cdw <- read_xlsx(path, sheet = "Relationships")

table(cdw$Entity1) # largest number of connections/edges: Surg_SurgeryINTRA
length(unique(cdw$Entity1)) # 98 views/tables in the DB

length(unique(cdw$Entity2)) # 129 nodes

length(unique(cdw$Link)) # 322 unique edges
length(cdw$Link) # 669 edges in total

#============================== 2D Networks ==================================#



#============================== 3D Networks ==================================#

#' @title Create 3-Dimensional Network Plot of CDW data views and tables
#' @param df dataframe with two columns, each containing nodes, where the relationship between column 1 and column 2 is from node1 to node2
#' @param slice selected CDW domain as a string. Internally passed to a grep call (case sensitive)
CDW3DNetwork <- function(df, slice) {
  require(igraph)
  require(rgl)
  g <- df[grep(slice, df[[1]]), ]
  g <- graph_from_data_frame(g)
  coords <- layout_with_fr(g, dim = 3)
  rglplot(g, layout = coords, vertex.color = "green", edge.color = "red")
}

#============================= End of Document  ==============================#