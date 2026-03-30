#' Geocontext: Efficient implementation of k-nearest neighbor geographical context analysis
#'
#' This implementation is based on the Equipop software [Malmberg et al.] and subsequent
#' Python implementation by Pontus Hennerdal. It significantly improves performance
#' by using a kd-tree method for distance matrix construction and efficient data.table
#' operations to replace record-by-record loops.
#'
#' @author Chris Fowler
#' @version 1.0
#' @date 2023-02-27
#'
#' @references 
#' P. Hennerdal, geocontext (2019), GitHub repository, https://github.com/PonHen/geocontext
#' 
#' @note This version is optimized for large datasets and memory efficiency

#' Required libraries
#' @import data.table
#' @import RANN
#' @import purrr
#' @import sf
suppressPackageStartupMessages({
  library(data.table)
  library(RANN)
  library(purrr)
  library(sf)
})

#' Calculate nearest neighbors and distance matrix
#' 
#' @description This function calculates the nearest neighbors and distance matrix 
#' for a given set of origins and destinations, where origin refers to the points for which 
#' neighbors are sought and destinations refers to the points that could be neighbors.
#'
#' @param origin data.table containing at least 'rn', 'X', and 'Y' columns (id and coordinates)
#' @param destination data.table containing at least 'rn', 'X', 'Y', and 'Total' columns (id, coordinates, and population)
#' @param needed integer, the number of neighbors to search for
#' @param kmax integer, the population threshold needed for inclusion
#'
#' @return list containing:
#'   \item{use}{the rn values from origin that meet the criteria of a population exceeding kmax}
#'   \item{nn}{the nearest neighbor matrix (note: values are row indices from destination, not rn values)}
#'   \item{dist}{the distance matrix for each value i in origin and its j nearest neighbors}
#'   \item{tot}{cumulative population totals (byproduct of determining which values meet kmax criteria)}
#'
#' @noRd
geocontext_Distance <- function(origin, destination, needed, kmax) {
  start.time <- Sys.time()
  message(sprintf("Constructing distance matrix with %d origin points and %d neighbors", 
                 nrow(origin), needed))
  
  # Cap the number of neighbors to avoid memory issues
  if (needed > 10000) {
    warning("Large k-value detected. Capping neighbors at 10000 for computational efficiency.")
    needed <- 10000
  }
  
  # Calculate nearest neighbors using kd-tree
  nnValues <- nn2(
    data = destination[, .(X, Y)],
    query = origin[, .(X, Y)],
    k = needed,
    treetype = 'kd',
    searchtype = 'standard',
    eps = 0.0
  )
  
  nn <- data.table(nnValues[[1]])
  nn$rn <- origin$rn
  setkey(nn, rn)
  
  # Create population lookup
  smallpop <- destination[, .(rn, Total)]
  smallpop$popLRow <- seq_len(nrow(destination))
  
  # Calculate cumulative populations
  tot <- nn
  tot <- tot[, melt(.SD, id.vars = 'rn')]
  tot <- tot[smallpop, new_value := i.Total, on = c(value = 'popLRow')][
    , dcast(.SD, rn ~ variable, value.var = 'new_value')
  ]
  
  # Convert to cumulative sums
  tot <- tot[, -1]  # drop row number
  
  if (nrow(tot) > 1) { 
    tot[, names(tot) := Reduce('+', tot, accumulate = TRUE)]
  } else {
    tot <- rbind(tot, tot)
    tot[, names(tot) := Reduce('+', tot, accumulate = TRUE)]
    tot <- tot[1, ]
  }
 
  # Identify points with sufficient population to meet kmax threshold
  setnames(x = tot, old = colnames(tot)[length(colnames(tot))], new = "MAX")
  tot$rn <- nn$rn
  use <- tot[MAX > kmax, rn]
  
  # Extract distances to neighbors
  dist <- setDT(as.data.frame(nnValues[[2]]))
  dist$rn <- origin$rn
  setkey(dist, rn)
  
  end.time <- Sys.time()
  message(sprintf("Distances and neighbors calculated. %.2f seconds elapsed", 
                 as.numeric(end.time - start.time, units = "secs")))
  
  return(list(use, nn, dist, tot))
}

#' Core geocontext calculation function
#'
#' @description Works on subsets of popLocations for which the nearest neighbor matrix 
#' is sufficient to exceed kmax. This allows efficient processing of large datasets by
#' constructing very large distance matrices for a much smaller set of points, 
#' dramatically reducing memory requirements and computation time.
#'
#' @param use vector of IDs (rn) from popLocations to process
#' @param nn nearest neighbor matrix from geocontext_Distance
#' @param dist distance matrix from geocontext_Distance
#' @param tot cumulative population matrix from geocontext_Distance
#' @param pop data.table containing population data
#' @param ks vector of k-values to calculate context for
#' @param gps vector of group column names to analyze
#'
#' @return data.table containing cell counts, populations, geocontext, and radius for each k-value
#'
#' @noRd
geocontext_Core <- function(use, nn, dist, tot, pop, ks, gps) {
  start.time <- Sys.time()
  message(sprintf("Begin Core subroutine with %d records", length(use)))
  
  # Subset to the points we're analyzing
  nn <- nn[rn %in% use, ]
  dist <- dist[rn %in% use, ]
  tot <- tot[rn %in% use, ]
  tot <- tot[, rn := NULL]
  
  # Calculate how many nearest neighbors to count for each k-value
  ktallies <- data.table(rn = nn$rn)
  for (i in seq_along(ks)) {
    ktallies[, c := apply(X = tot, MARGIN = 1, FUN = function(x) 
      detect_index(x, function(x) {x >= ks[i]}))]
    setnames(ktallies, old = 'c', new = paste0("K", ks[i]))
  }
  setkey(ktallies, rn)
  
  # Reshape to long format and add grouping ID
  molten_neighbors <- melt(nn, id.vars = 'rn')[, grp_id := .GRP, by = variable]
  molten_distance <- melt(dist, id.vars = 'rn')[, grp_id := .GRP, by = variable]
  setkey(molten_neighbors, rn)
  setkey(molten_distance, rn)
  
  # Create results data.table
  res <- pop[rn %in% use, ]
  
  # Process each k-value
  for (i in 2:(length(ks) + 1)) {  # Start at second column to skip rn column
    # Create variable names
    ti.tle <- paste0("K", ks[i-1])
    gr.oups <- paste0(ti.tle, "_", gps)
    co.ls <- c('rn', paste0(ti.tle, "_Total"), gr.oups, paste0(ti.tle, "_radius"))
    
    # Temporary data table for this k-value
    add <- ktallies[, c('rn', ..ti.tle)]
    
    # Join by rn and calculate sums
    add <- add[
      molten_neighbors, 
      on = c('rn', paste0(ti.tle, ">=grp_id")), 
      nomatch = 0L
    ][
      pop[, c('rn', 'Total', ..gps)], 
      on = .(value = rn), 
      c('rn', 'Total', ..gps),
      nomatch = 0L
    ][
      , lapply(.SD, sum), keyby = rn
    ]
    
    # Get radius values
    dist_k <- ktallies[, c('rn', ..ti.tle)]
    dist_k <- dist_k[
      molten_distance, 
      on = c('rn', paste0(ti.tle, "==grp_id")),
      nomatch = 0L
    ][, .(rn, value)]
    setkey(dist_k, rn)
    
    # Merge population counts and radius
    add <- merge(add, dist_k)
    colnames(add) <- co.ls
    
    # Add to the result table
    res <- merge(res, add)
  }
  
  end.time <- Sys.time()
  message(sprintf("Subroutine complete for %d records after %.2f seconds", 
                 length(use), 
                 as.numeric(end.time - start.time, units = "secs")))
  return(res)
}

#' Calculate geographical context around points
#'
#' @description This function calculates the geographical context around each point in 
#' the 'popLocations' data.frame. The context is the count of the population among 
#' the k-nearest neighbours in the dataframe 'popLocations' that is part of a group 
#' counted in that data frame. Distance is measured as Euclidean distance.
#'
#' @param popLocations data.frame with coordinates (X, Y), population values (Total), 
#'        and group populations
#' @param groups vector of strings naming columns holding values for population groups
#' @param kValues vector of integers being the k in k-nearest neighbors
#'
#' @return data.table containing original data plus geocontext metrics for each k-value
#'
#' @examples
#' \dontrun{
#' data <- read.csv("sample_data.csv")
#' data$rn <- 1:nrow(data)
#' colnames(data)[colnames(data) == "x_coord"] <- "X"
#' colnames(data)[colnames(data) == "y_coord"] <- "Y"
#' colnames(data)[colnames(data) == "pop_total"] <- "Total"
#' groups <- c("pop_group1", "pop_group2")
#' k_values <- c(400, 1600, 6400)
#' result <- geocontext(popLocations = data, groups = groups, kValues = k_values)
#' }
#'
#' @export
geocontext <- function(popLocations, groups, kValues) {
  # Start the stopwatch
  time0 = Sys.time()
  message(sprintf("Begin computation at %s", format(time0)))
  
  # Input validation
  if (!is.data.frame(popLocations)) {
    stop("popLocations must be a data.frame")
  }
  if (!all(c("X", "Y", "Total", "rn") %in% names(popLocations))) {
    stop("popLocations must contain columns 'X', 'Y', 'Total', and 'rn'")
  }
  if (!all(groups %in% names(popLocations))) {
    stop("Not all group columns found in popLocations")
  }
  
  # Convert to data.table for speed
  setDT(popLocations, key = 'rn')
  
  # Estimate needed neighbors based on population density
  cellPop <- 0.5 * mean(popLocations[, Total], na.rm = TRUE)
  needed <- ceiling(max(kValues) / cellPop)
  if (needed > 1000) {
    needed <- 1000  # Cap initial matrix size
  }
  
  # First round: Calculate distances and determine qualifying points
  firstSubroutine <- geocontext_Distance(
    origin = popLocations[, .(rn, X, Y)],
    destination = popLocations[, .(rn, X, Y, Total)],
    needed = needed,
    kmax = max(kValues)
  )
  
  use <- firstSubroutine[[1]]
  nn <- firstSubroutine[[2]]
  dist <- firstSubroutine[[3]]
  tot <- firstSubroutine[[4]]
  
  # Calculate geocontext for qualified points
  result <- geocontext_Core(
    use = use, 
    nn = nn, 
    dist = dist, 
    tot = tot, 
    pop = popLocations, 
    ks = kValues, 
    gps = groups
  )
  
  # Process remaining points with increasingly larger distance matrices
  loops <- c(3 * needed, 10 * needed, nrow(popLocations))
  looper <- 1
  remainder <- popLocations[rn %in% use == FALSE, ]
  
  while (nrow(remainder) > 0 && looper <= length(loops)) {
    message(sprintf(
      "Processing %d remaining records with %d neighbors", 
      nrow(remainder), 
      loops[looper]
    ))
    
    nextSubroutine <- geocontext_Distance(
      origin = remainder[, .(rn, X, Y)],
      destination = popLocations[, .(rn, X, Y, Total)],
      needed = loops[looper],
      kmax = max(kValues)
    )
    
    use2 <- nextSubroutine[[1]]
    nn <- nextSubroutine[[2]]
    dist <- nextSubroutine[[3]]
    tot <- nextSubroutine[[4]]
    
    # Only proceed if we found qualifying points
    if (length(use2) > 0) {
      result2 <- geocontext_Core(
        use = use2, 
        nn = nn, 
        dist = dist, 
        tot = tot, 
        pop = popLocations, 
        ks = kValues, 
        gps = groups
      )
      
      result <- rbind(result, result2)
      use <- c(use, use2)
    }
    
    remainder <- popLocations[rn %in% use == FALSE, ]
    looper <- looper + 1
  }
  
  # Report any unprocessed records
  if (nrow(remainder) > 0) {
    warning(sprintf("%d records could not be processed with available neighbors", nrow(remainder)))
  }
  
  # Final results
  elapsed <- difftime(Sys.time(), time0, units = "mins")
  message(sprintf(
    "Process complete at %s after %.2f minutes", 
    format(Sys.time()), 
    as.numeric(elapsed)
  ))
  
  return(result)
}

#' Convert cell IDs to polygons for visualization
#'
#' @description This function creates polygon geometries for grid cells to enable
#' visualization using sf package.
#'
#' @param data data.frame containing the data to visualize
#' @param Lon x-coordinate column name
#' @param Lat y-coordinate column name
#' @param Size cell size (width/height)
#' @param Loc location of the coordinate ("centroid" or "corner")
#' @param Projection coordinate reference system (CRS)
#'
#' @return sf object with polygons representing grid cells
#'
#' @examples
#' \dontrun{
#' result_sf <- cellIDToPoly(
#'   data = results, 
#'   Lon = "X", 
#'   Lat = "Y", 
#'   Size = 1000, 
#'   Loc = "centroid",
#'   Projection = 3006
#' )
#' plot(result_sf["K400_Total"])
#' }
#'
#' @export
cellIDToPoly <- function(data, Lon, Lat, Size, Loc = "corner", Projection = 4326) {
  # Create points data.frame
  dist <- Size
  pts <- data.frame(Lon = data[[Lon]], Lat = data[[Lat]], dist = Size, Size = Size)
  
  # Create unique ID for each cell
  data$id <- apply(pts, MARGIN = 1, FUN = function(x) 
    paste0(x["Lon"], "_", x["Lat"], "_", x["Size"]))
  
  # Adjust coordinates if needed (for centroid-based locations)
  if (Loc == "centroid") {
    pts$Lon <- pts$Lon - (pts$dist * 0.5)
    pts$Lat <- pts$Lat - (pts$dist * 0.5)
  }
  
  # Create polygons from coordinates
  polys <- apply(pts, MARGIN = 1, FUN = function(x) {
    return(list(matrix(c(
      x["Lon"], x["Lat"],
      x["Lon"], x["Lat"] + x["dist"],
      x["Lon"] + x["dist"], x["Lat"] + x["dist"],
      x["Lon"] + x["dist"], x["Lat"],
      x["Lon"], x["Lat"]
    ), ncol = 2, byrow = TRUE)))
  })
  
  # Convert to sf polygons
  polys2 <- lapply(polys, FUN = function(x) sf::st_polygon(x))
  data$geometry <- sf::st_sfc(polys2)
  data <- sf::st_as_sf(data, crs = Projection)
  
  return(data)
}
