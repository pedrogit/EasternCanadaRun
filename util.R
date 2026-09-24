plotLeaflet <- function(...,
                       basemap = leaflet::providers$OpenStreetMap,
                       colors = NULL,
                       layerNames = NULL,
                       fillOpacity = 0.2,
                       weight = 2) {
  objs <- list(...)

  if (length(objs) == 0L) {
    stop("plotLeaflet requires at least one spatial object.")
  }

  if (length(objs) == 1L && is.list(objs[[1]]) && !inherits(objs[[1]], c("SpatVector", "sf", "sfc"))) {
    objs <- objs[[1]]
  }

  if (is.null(layerNames)) {
    layerNames <- names(objs)
    if (is.null(layerNames) || any(layerNames == "")) {
      layerNames <- paste0("Layer ", seq_along(objs))
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

  x_sf_list <- lapply(objs, function(obj) {
    x_sf <- if (inherits(obj, "SpatVector")) {
      sf::st_as_sf(obj)
    } else {
      obj
    }

    if (inherits(obj, "SpatVector") && terra::crs(obj) != "") {
      sf::st_crs(x_sf) <- terra::crs(obj)
    }

    crs_x <- sf::st_crs(x_sf)
    if (!is.na(crs_x$wkt) && !sf::st_is_longlat(x_sf)) {
      x_sf <- sf::st_transform(x_sf, 4326)
    }

    x_sf
  })

  all_bbox <- do.call(rbind, lapply(x_sf_list, sf::st_bbox))
  bounds <- unname(c(
    min(all_bbox[, "xmin"]),
    min(all_bbox[, "ymin"]),
    max(all_bbox[, "xmax"]),
    max(all_bbox[, "ymax"])
  ))

  map <- leaflet::leaflet() |>
    leaflet::addProviderTiles(basemap)

  for (i in seq_along(x_sf_list)) {
    map <- map |>
      leaflet::addPolygons(
        data = x_sf_list[[i]],
        group = layerNames[i],
        color = colors[i],
        fillColor = colors[i],
        fillOpacity = fillOpacity,
        weight = weight,
        stroke = TRUE
      )
  }

  map |>
    leaflet::addLayersControl(
      overlayGroups = layerNames,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::fitBounds(bounds[1], bounds[2], bounds[3], bounds[4])
}
