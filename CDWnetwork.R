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

#==================== Relationship data ====================#
path <- "I:/Sears-CDA/3. Project Management/CDW_DB/ORD_Sears_201803016D.xlsx"

###### Data I created from the DB using the metadata views
cdw <- read_xlsx(path, sheet = "Relationships")

table(cdw$Entity1) # largest number of connections/edges: Surg_SurgeryINTRA
length(unique(cdw$Entity1)) # 98 views/tables in the DB

length(unique(cdw$Entity2)) # 129 nodes

length(unique(cdw$Link)) # 322 unique edges
length(cdw$Link) # 669 edges in total

###### VINCI data
metaVINCI <- read_xlsx(path, sheet = "MetaVINCI")

table(metaVINCI$Entity1) 
# largest number of edges: SStaff_SFeeServiceProvided
# tied with Fee_FeeServiceProvided

length(unique(metaVINCI$Entity1))
# 100 views/tables in the DB

length(unique(metaVINCI$Entity2))
# 125 nodes

length(unique(metaVINCI$Link))
# 323 unique edges

length(metaVINCI$Link)
# 720 edges in total (same as dim(metaVINCI)[1])

#==================== Nodes ====================#

views <- read_xlsx(path, sheet = "Views")

nodes <- views %>%
  select(ViewName, RowCounts, Domains, PrimaryKey) %>%
  rename(node = ViewName)

edges <- cdw %>%
  rename(
    From = Entity1,
    To = Entity2
  )

all_sets <- data.frame(
  nodes = c(unique(edges$From), 
              unique(edges$To)),
  stringsAsFactors = FALSE
  ) %>%
  distinct(nodes) %>%
  filter(!is.na(nodes)) %>%
  mutate(id = as.character(row_number()))
  


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