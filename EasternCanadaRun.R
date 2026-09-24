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

source(file.path(basePath, "EasternCanadaRun/util.R"))

# Required by prepInputs()/module .inputObjects() when it downloads/caches data.
options(reproducible.destinationPath = file.path(basePath, "inputs"))

# =========================================================
# READ NEWFOUNDLAND & LABRADOR BOUNDARIES
# =========================================================
nl <- terra::vect(file.path(basePath, "EasternCanadaRun/data/NL_EB_Poly_50k_Upload.shp"))

# Select only one of them
nl_gf <- nl[nl$DIST_NAME == "Grand Falls-Windsor - Buchans", ]

# Make a 10km buffer around the center of it and project it to WGS 84
studyArea <- terra::project(terra::buffer(terra::centroids(nl_gf), width=10000), "EPSG:4326")

plotLeaflet(studyArea)

# =========================================================
# MODULES
# =========================================================

modules <- c(
  "EasternCanadaDataPrep",
  "RiparianBuffers",
  "EasternCanadaLandbase"
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
    studyArea = studyArea
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
