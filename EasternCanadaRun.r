gc()

library(SpaDES.core)
library(SpaDES.project)
library(terra)
library(sf)

# =========================================================
# PATHS
# =========================================================
basePath <- "~/repositories/Shirin/"
setPaths(
  cachePath   = file.path(basePath, "cache"),
  inputPath   = file.path(basePath, "inputs"),
  outputPath  = file.path(basePath, "outputs"),
  modulePath  = file.path(basePath, "modules"),
  scratchPath = file.path(basePath, "scratch")
)

# Required by prepInputs()/module .inputObjects() when it downloads/caches data.
options(reproducible.destinationPath = file.path(basePath, "inputs"))

# =========================================================
# READ NEWFOUNDLAND & LABRADOR BOUNDARIES
# =========================================================
nl <- st_read(file.path(basePath, "inputs/NL_EB_Poly_50k_Upload.shp"), quiet = TRUE)

# Unnecessary
# nl <- st_make_valid(nl)

# =========================================================
# SELECT NEWFOUNDLAND ISLAND AUTOMATICALLY
# =========================================================
# The current YCF_NL layer covers Newfoundland Island,
# but not Labrador.
#
# Newfoundland Island is south of Labrador.
# We therefore select the southern polygons automatically.
# =========================================================
#  project to WGS 84
nl_ll <- st_transform(nl, 4326)

centroids <- st_point_on_surface(nl_ll)
coords <- st_coordinates(centroids)

nl_ll$longitude <- coords[, 1]
nl_ll$latitude  <- coords[, 2]

# Newfoundland Island is approximately south of 52 N
newfoundlandIsland <- nl_ll[
  nl_ll$latitude < 52,
]

if (nrow(newfoundlandIsland) == 0) {
  stop("Could not identify Newfoundland Island polygons.")
}

cat(
  "Newfoundland Island polygons found:",
  nrow(newfoundlandIsland),
  "\n"
)

# =========================================================
# SELECT ONE POLYGON FOR SMALL TEST
# =========================================================
# Choose a polygon near the middle of Newfoundland Island
targetLat <- median(newfoundlandIsland$latitude)

i <- which.min(
  abs(newfoundlandIsland$latitude - targetLat)
)

testPolygon <- newfoundlandIsland[i, ]

# Return to original CRS
testPolygon <- st_transform(
  testPolygon,
  st_crs(nl)
)

cat(
  "Selected district:",
  testPolygon$DIST_NAME,
  "\n"
)

# =========================================================
# CREATE 5 x 5 km TEST PATCH
# =========================================================
testPoint <- st_point_on_surface(
  st_union(testPolygon)
)

xy <- st_coordinates(testPoint)

small_ext <- terra::ext(
  xy[1] - 2500,
  xy[1] + 2500,
  xy[2] - 2500,
  xy[2] + 2500
)

small_box <- terra::as.polygons(
  small_ext,
  crs = terra::crs(
    terra::vect(testPolygon)
  )
)

small_poly <- terra::intersect(
  terra::vect(testPolygon),
  small_box
)

if (nrow(small_poly) == 0) {
  stop("The test patch is empty.")
}

# =========================================================
# CHECK TEST AREA
# =========================================================
cat("\n====================================\n")
cat("TEST AREA\n")
cat("====================================\n")

print(small_poly)

cat("\nExtent:\n")
print(terra::ext(small_poly))

plot(small_poly)

# plot with leaflet and a basemap
plotWithLeaflet <- function(data) {
  leaflet::leaflet() |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap) |>
    leaflet::addPolygons(data = st_transform(st_as_sf(data), 4326))
}

plotWithLeaflet(small_poly)

# =========================================================
# MODULES
# =========================================================
modules <- c(
  "EasternCanadaDataPrep"
  #, "RiparianBuffers"
  #, "EasternCanadaLandbase"
)

# =========================================================
# INIT
# =========================================================
sim <- simInit(
  times = list(
    start = 1,
    end = 1
  ),
  modules = modules,
  objects = list(
    studyArea = small_poly
  ),
  params = list(
    EasternCanadaDataPrep = list(
      devMode = FALSE
    ),
    RiparianBuffers = list(
      hydroRaster_m = 25
    )
  ),
  paths = getPaths()
)


# =========================================================
# RUN
# =========================================================

system.time({
  sim <- spades(sim)
})


# =========================================================
# CHECK OUTPUTS
# =========================================================

cat("\n====================================\n")
cat("OUTPUT OBJECTS\n")
cat("====================================\n")

print(names(sim))


# =========================================================
# PLANNING GRID
# =========================================================

cat("\nPlanningGrid resolution:\n")
print(
  terra::res(sim$PlanningGrid)
)

cat("\nPlanningGrid extent:\n")
print(
  terra::ext(sim$PlanningGrid)
)


# =========================================================
# YIELD CURVE FAMILY
# =========================================================

if ("yieldCurveFamily" %in% names(sim)) {

  cat("\n====================================\n")
  cat("YIELD CURVE FAMILY\n")
  cat("====================================\n")

  print(
    terra::freq(sim$yieldCurveFamily)
  )

  print(
    sim$yieldCurveLookup
  )
}


# =========================================================
# LANDBASE CHECKS
# =========================================================

cat("\n====================================\n")
cat("LANDBASE CHECKS\n")
cat("====================================\n")

print(
  terra::global(
    sim$forestCoverMask,
    "sum",
    na.rm = TRUE
  )
)

print(
  terra::global(
    sim$protectedAreaMask,
    "sum",
    na.rm = TRUE
  )
)

print(
  terra::global(
    sim$Riparian$riparianFraction,
    c("min", "max"),
    na.rm = TRUE
  )
)

print(
  terra::global(
    sim$harvestableFraction,
    c("min", "max"),
    na.rm = TRUE
  )
)

print(
  terra::global(
    sim$harvestableFraction,
    "sum",
    na.rm = TRUE
  )
)


# =========================================================
# PLOTS
# =========================================================

plot(sim$PlanningGrid)

if ("yieldCurveFamily" %in% names(sim)) {
  plot(sim$yieldCurveFamily)
}

plot(sim$forestCoverMask)

plot(sim$protectedAreaMask)

plot(sim$Riparian$riparianFraction)

plot(sim$harvestableFraction)
