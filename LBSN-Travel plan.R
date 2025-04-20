# Load libraries
library(readxl)
library(maps)
library(geosphere)
library(mapdata)
library(leaflet)
library(igraph)

# Load dataset 
dataset <- read_excel("D:/TA_VGI_1402/final_dataset/final_data/UK_Gowalla_checkins.xlsx")
friend <- read.table("D:/TA_VGI_1402/final_dataset/final_data/Gowalla_edges.txt")

# Check missing values
any(is.na(dataset))

# Rename columns of the dataset
names(dataset) <- c("id", "time", "lat", "lon", "location_id")

# Convert the 'time' column to another format
dataset$time <- as.numeric(as.character(dataset$time)) 
dataset$time <- as.POSIXct(dataset$time)
dataset$datetime <- format(dataset$time, "%Y-%m-%d %H:%M:%S")

# Extract unique ID and datetime
unique_values <- unique(dataset[, c("id", "datetime","lat","lon")])
data <- dataset[!duplicated(dataset[c("id", "datetime","lat","lon")]), ]

# Extract user locations 
locations <- function(ID) {
  rows <- which(data$id == ID)
  location <- list()
  sorted_rows <- rows[order(data$datetime[rows])]
  for (row in sorted_rows) {
    location[[row]] <- list(
      datetime = data$datetime[row],
      lat = data$lat[row],
      lon = data$lon[row],
      location_id = data$location_id[row]
    )
  }
  return(location)
}

# Test user
test <- 340
l <- locations(test)
lat <- c()
lon <- c()
for (row in l) {
  lat <- c(lat, row$lat)
  lon <- c(lon, row$lon)
}

# User location map
mymap <- leaflet() %>% addTiles()
min_lon <- min(lon)
min_lat <- min(lat)
max_lon <- max(lon)
max_lat <- max(lat)
mymap <- mymap %>% fitBounds(min_lon, min_lat, max_lon, max_lat)
mymap <- mymap %>% addMarkers(lng = lon, lat = lat)
mymap <- mymap %>% addCircleMarkers(lng = lon, lat = lat, label = "user", labelOptions = labelOptions(textsize = "20px"), radius = 20, fillColor = "red")
mymap

# Find friend with degree 1 
unique_id <- unique(data$id)
english_rows <- which(friend[, 1] %in% unique_id)
friend <- friend[english_rows, ]
english_rows <- which(friend[, 2] %in% unique_id)
friend <- friend[english_rows, ]

g <- graph_from_data_frame(d = friend, directed = FALSE)
adj_matrix <- as_adjacency_matrix(g)
node_names <- V(g)$name
node_data <- which(node_names==test)
columns <- which(adj_matrix[node_data, ] == 2)
freind_id <- node_names[columns]

# Freind location map
for (row in freind_id) {
  l_f <- locations(row)
  lat_f <- c()
  lon_f <- c()
  for (r in l_f) {
    lat_f <- c(lat_f, r$lat)
    lon_f <- c(lon_f, r$lon)
  }
  mymap <- mymap %>% addMarkers(lng = lon_f, lat = lat_f)
}
mymap


#---------------------------------------------------------------
#intersection location user and friend
intersection_count <- c()
user_coords <- paste(lat, lon, sep = ",")
for (i in seq_along(freind_id)) {
  l_f2 <- locations(freind_id[i])  
  lat_f2 <- c()
  lon_f2 <- c()
  for (r in l_f2) {
    lat_f2 <- c(lat_f2, r$lat)
    lon_f2 <- c(lon_f2, r$lon)
  }
  fri_coords <- paste(lat_f2, lon_f2, sep = ",")
  common_coords <- intersect(user_coords, fri_coords)
  intersection_count <- c(intersection_count, length(common_coords))
}
intersection_count



#mean distances between test user and friends
mean_distances <- c()
for (i in freind_id) {
  l_f2 <- locations(i)
  lat_f2 <- c()
  lon_f2 <- c()
  for (r in l_f2) {
    lat_f2 <- c(lat_f2, r$lat)
    lon_f2 <- c(lon_f2, r$lon)
  }
  fri <- data.frame(
    lat = lat_f2,
    lon = lon_f2
  )
  distances <- apply(fri, 1, function(x) geosphere::distHaversine(data.frame(lat = lat, lon = lon), x))
  mean_distance <- mean(distances)
  mean_distances <- c(mean_distances, mean_distance)
}

freind_id
mean_distances


#--------------------------------------------------------------------
library(igraph)
library(dplyr)


#creat graph
g <- make_empty_graph(directed = TRUE)

#add node
unique_locations <- unique(data[, c("lat", "lon")])
g <- add_vertices(g, nv = nrow(unique_locations), name = 1:nrow(unique_locations))


#sort time
rows <- 1:nrow(data)
sorted_rows <- rows[order(data$datetime[rows], data$id[rows])]
sorted_data <- data[sorted_rows, ]

#add edges 
for (i in 1:(nrow(sorted_data) - 1)) {
  from_location <- which(unique_locations$lat == sorted_data$lat[i] & unique_locations$lon == sorted_data$lon[i])
  if (sorted_data$id[i] == sorted_data$id[i + 1]) {
    to_location <- which(unique_locations$lat == sorted_data$lat[i + 1] & unique_locations$lon == sorted_data$lon[i + 1])
    g <- add_edges(g, c(from_location, to_location), edge.attr = list(name = paste0("edge_", i)))
  }
}



#edge wights
edge_weights <- rep(0, ecount(g))  
for (i in 1:(ecount(g))) {
  edges <- get.edgelist(g)[i, ]
  from <- edges[1]
  to <- edges[2]
  valid_rows <- subset(sorted_data, 
                     lat == unique_locations$lat[from] & 
                     lon == unique_locations$lon[from] & 
                     lead(lat, default = NULL) == unique_locations$lat[to] & 
                     lead(lon, default = NULL) == unique_locations$lon[to] & 
                     lead(id, default = NULL) == id)
  edge_weights[i] <- nrow(valid_rows)

}

E(g)$weight <- edge_weights
max(edge_weights)


#-------------------------------javabiat har yal-----------------------
#---------------------test------------
edge_number <- 8
from_node <- ends(g, es = edge_number)[1]
to_node <- ends(g, es = edge_number)[2]
connected_edges <- c(which(get.edgelist(g)[,1] == from_node), which(get.edgelist(g)[,2] == to_node))
connected_edges

related_nodes <- unique(c(ends(g, es = connected_edges)[,1], ends(g, es = connected_edges)[,2]))
subgraph <- induced_subgraph(g, related_nodes)
plot(subgraph, edge.color = "black", vertex.color = "lightblue", 
     vertex.label.dist = 0.5, main = "Subgraph with Selected Edges")

#vazn har yal in or out
from_node <- 7365
to_node <- 15292
edge_number <- which(get.edgelist(g)[,1] == from_node & get.edgelist(g)[,2] == to_node)
E(g)[edge_number]$weight

#tedad in or out
from_node <- 15292
edge_numbers <- which(get.edgelist(g)[,2] == from_node)
num_edges <- length(edge_numbers)
num_edges
#-----------------------------------

#sum in and out weights
total_out_weights <- rep(0, vcount(g))
total_in_weights <- rep(0, vcount(g))
for (i in 1:ecount(g)) {
  edges <- get.edgelist(g)[i, ]
  from <- edges[1]
  to <- edges[2]
  total_out_weights[from] <- total_out_weights[from] + edge_weights[i]
  total_in_weights[to] <- total_in_weights[to] + edge_weights[i]
}



#jazabiat
for (i in 1:ecount(g)) {
  from <- ends(g, es = i)[1]
  to <- ends(g, es = i)[2]
  out <- edge_weights[i] / total_out_weights[from]
  into <- edge_weights[i] / total_in_weights[to]
  new_weight <- edge_weights[i] * (into + out)
  edge_weights[i] <- new_weight
}

E(g)$weight <- edge_weights
edge_number <- 8
E(g)[edge_number]$weight


#----------------------------
#yal ba jazabiat 2
sum(E(g)$weight > 0)
graph <- g
edges_to_delete <- which(E(graph)$weight < 0.5)
sum(E(graph)$weight >0)
graph <- delete_edges(graph, edges_to_delete)
#------------------------------


is_connected(g)
subgraphs <- decompose.graph(g)
num_subgraphs <- length(subgraphs)
num_subgraphs


for (subgraph in subgraphs) {
  print(vcount(subgraph)) 
  print(ecount(subgraph)) 
}


large_subgraphs <- Filter(function(subgraph) {
  vcount(subgraph) > 9
}, subgraphs)







#method1 -----------------------
max_path_weight <- -Inf
max_path_weight <- -Inf
max_path <- NULL
for (subgraph in large_subgraphs) {  
  source_node <- which(degree(subgraph, mode = "in") == 0)
  all_paths <- all_simple_paths(subgraph, from = source_node, mode = "out")
  for (path in all_paths) {
    path_weight <- sum(E(subgraph, path = path)$weight)
    if (path_weight > max_path_weight) {
      max_path_weight <- path_weight
      max_path <- path
    }
  }
}

max_path


#---------------- method 2
max_path_weight <- -Inf
max_path <- NULL
for (subgraph in large_subgraphs) {
  all_paths <- list()
  for (source_node in 1:vcount(subgraph)) {
    paths <- all_simple_paths(subgraph, from = source_node, mode = "out")
    all_paths <- c(all_paths, paths)
  }
  
  for (path in all_paths) {
    path_weight <- sum(E(subgraph, path = path)$weight)
    if (path_weight > max_path_weight) {
      max_path_weight <- path_weight
      max_path <- path
    }
  }
}

max_path

