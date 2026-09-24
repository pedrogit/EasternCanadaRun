plotLeaflet <- function(...,
                       basemap = leaflet::providers$OpenStreetMap,
                       colors = NULL,
                       layerNames = NULL,
                       fillOpacity = 0.2,
                       weight = 2) {
  exprs <- as.list(substitute(list(...)))[-1L]
  objs <- list(...)

  if (length(objs) == 0L) {
    stop("plotLeaflet requires at least one spatial object.")
  }

  if (length(objs) == 1L && is.list(objs[[1]]) && !inherits(objs[[1]], c("SpatVector", "sf", "sfc"))) {
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

  non_empty <- vapply(x_sf_list, function(x) nrow(x) > 0, logical(1))
  dropped_layers <- layerNames[!non_empty]

  if (length(dropped_layers) > 0L) {
    warning(
      paste0("Empty layer ", dropped_layers, " was not added to the map"),
      call. = FALSE
    )
  }

  x_sf_list <- x_sf_list[non_empty]
  layerNames <- layerNames[non_empty]
  colors <- colors[non_empty]

  if (length(x_sf_list) == 0L) {
    stop("No non-empty layers to plot after filtering.")
  }

  all_bbox <- do.call(rbind, lapply(x_sf_list, sf::st_bbox))
  bounds <- unname(c(
    min(all_bbox[, "xmin"], na.rm = TRUE),
    min(all_bbox[, "ymin"], na.rm = TRUE),
    max(all_bbox[, "xmax"], na.rm = TRUE),
    max(all_bbox[, "ymax"], na.rm = TRUE)
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
