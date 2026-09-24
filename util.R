plotLeaflet <- function(...,
                        basemap = leaflet::providers$OpenStreetMap,
                        colors = NULL,
                        layerNames = NULL,
                        fillOpacity = 0.2,
                        weight = 2,
                        rasterOpacity = 0.7,
                        rasterMaxCells = 5e5) {
  exprs <- as.list(substitute(list(...)))[-1L]
  objs <- list(...)

  if (length(objs) == 0L) {
    stop("plotLeaflet requires at least one spatial object.")
  }

  if (length(objs) == 1L && is.list(objs[[1]]) &&
      !inherits(objs[[1]], c("SpatVector", "SpatRaster", "sf", "sfc"))) {
    objs <- objs[[1]]

    if (is.null(layerNames)) {
      layerNames <- names(objs)
      if (is.null(layerNames) || any(layerNames == "")) {
        layerNames <- paste0("Layer ", seq_along(objs))
      }
    }
  }

  if (is.null(layerNames)) {
    layerNames <- names(objs)

    if (is.null(layerNames) || any(layerNames == "")) {
      exprNames <- vapply(exprs, function(e) paste(deparse(e), collapse = ""), character(1))
      exprNames[exprNames == ""] <- NA_character_

      if (length(exprNames) == length(objs)) {
        layerNames <- exprNames
      }

      if (is.null(layerNames) || any(is.na(layerNames) | layerNames == "")) {
        missing_idx <- which(is.na(layerNames) | layerNames == "")
        layerNames[missing_idx] <- paste0("Layer ", missing_idx)
      }
    }
  }

  if (length(layerNames) != length(objs)) {
    stop("The number of layerNames must match the number of spatial objects.")
  }

  if (is.null(colors)) {
    colors <- grDevices::hcl.colors(length(objs), palette = "Dark 2")
  } else if (length(colors) == 1L) {
    colors <- rep(colors, length(objs))
  }

  if (length(colors) != length(objs)) {
    stop("The number of colors must match the number of spatial objects.")
  }

  layer_info <- vector("list", length(objs))

  for (i in seq_along(objs)) {
    obj <- objs[[i]]

    if (inherits(obj, "SpatVector") || inherits(obj, c("sf", "sfc"))) {
      x_sf <- if (inherits(obj, "SpatVector")) sf::st_as_sf(obj) else obj
      if (inherits(obj, "SpatVector") && terra::crs(obj) != "") {
        sf::st_crs(x_sf) <- terra::crs(obj)
      }

      crs_x <- sf::st_crs(x_sf)
      if (!is.na(crs_x$wkt) && !sf::st_is_longlat(x_sf)) {
        x_sf <- sf::st_transform(x_sf, 4326)
      }

      layer_info[[i]] <- list(type = "vector", data = x_sf)

    } else if (inherits(obj, "SpatRaster")) {
      r <- obj
      if (terra::crs(r) == "") {
        stop("SpatRaster layer has no CRS: ", layerNames[i])
      }
      if (!terra::is.lonlat(r)) {
        r <- terra::project(r, "EPSG:4326", method = "near")
      }

      ncell_original <- terra::ncell(r)
      if (is.finite(rasterMaxCells) && rasterMaxCells > 0) {
        while (terra::ncell(r) > rasterMaxCells) {
          agg_fun <- if (any(terra::is.factor(r))) terra::modal else mean
          r <- terra::aggregate(r, fact = 2, fun = agg_fun, na.rm = TRUE)
        }
      }

      if (terra::ncell(r) < ncell_original) {
        warning(
          paste0(
            "Raster layer ", layerNames[i], " was downsampled from ",
            format(ncell_original, big.mark = ","), " to ",
            format(terra::ncell(r), big.mark = ","), " cells before plotting"
          ),
          call. = FALSE
        )
      }

      layer_info[[i]] <- list(type = "raster", data = r)

    } else {
      stop("Unsupported layer class for ", layerNames[i], ".")
    }
  }

  keep <- rep(TRUE, length(layer_info))

  for (i in seq_along(layer_info)) {
    li <- layer_info[[i]]

    if (identical(li$type, "vector") && nrow(li$data) == 0) {
      keep[i] <- FALSE
      warning(paste0("Empty layer ", layerNames[i], " was not added to the map"), call. = FALSE)
      next
    }

    if (identical(li$type, "raster")) {
      e <- terra::ext(li$data)
      bounds_i <- c(terra::xmin(e), terra::ymin(e), terra::xmax(e), terra::ymax(e))
      if (!all(is.finite(bounds_i))) {
        keep[i] <- FALSE
        warning(paste0("Empty layer ", layerNames[i], " was not added to the map"), call. = FALSE)
      }
    }
  }

  layer_info <- layer_info[keep]
  layerNames <- layerNames[keep]
  colors <- colors[keep]

  if (length(layer_info) == 0L) {
    stop("No non-empty layers to plot after filtering.")
  }

  bbox_list <- lapply(layer_info, function(li) {
    if (identical(li$type, "vector")) {
      b <- sf::st_bbox(li$data)
      c(
        xmin = as.numeric(b["xmin"]),
        ymin = as.numeric(b["ymin"]),
        xmax = as.numeric(b["xmax"]),
        ymax = as.numeric(b["ymax"])
      )
    } else {
      e <- terra::ext(li$data)
      c(xmin = terra::xmin(e), ymin = terra::ymin(e), xmax = terra::xmax(e), ymax = terra::ymax(e))
    }
  })

  xmin_vals <- vapply(bbox_list, function(b) as.numeric(b["xmin"]), numeric(1))
  ymin_vals <- vapply(bbox_list, function(b) as.numeric(b["ymin"]), numeric(1))
  xmax_vals <- vapply(bbox_list, function(b) as.numeric(b["xmax"]), numeric(1))
  ymax_vals <- vapply(bbox_list, function(b) as.numeric(b["ymax"]), numeric(1))

  bounds <- c(
    min(xmin_vals, na.rm = TRUE),
    min(ymin_vals, na.rm = TRUE),
    max(xmax_vals, na.rm = TRUE),
    max(ymax_vals, na.rm = TRUE)
  )

  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(basemap)

  for (i in seq_along(layer_info)) {
    li <- layer_info[[i]]

    if (identical(li$type, "vector")) {
      map <- map |>
        leaflet::addPolygons(
          data = li$data,
          group = layerNames[i],
          color = colors[i],
          fillColor = colors[i],
          fillOpacity = fillOpacity,
          weight = weight,
          stroke = TRUE
        )
    } else {
      mm <- terra::minmax(li$data)
      rng <- range(mm, na.rm = TRUE)
      if (!all(is.finite(rng))) {
        rng <- c(0, 1)
      }

      pal <- leaflet::colorNumeric(
        palette = grDevices::hcl.colors(64, "YlOrRd", rev = TRUE),
        domain = rng,
        na.color = "transparent"
      )

      map <- map |>
        leaflet::addRasterImage(
          x = li$data,
          colors = pal,
          opacity = rasterOpacity,
          group = layerNames[i],
          project = FALSE
        )
    }
  }

  map <- map |>
    leaflet::addLayersControl(
      overlayGroups = layerNames,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    )

  if (all(is.finite(bounds))) {
    map <- map |>
      leaflet::fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
  }

  map
}
