## ---- echo=FALSE---------------------------------------------------------
if (!requireNamespace("plotly", quietly = TRUE)) {
  stop("plotly needed for this vignette to build.",
    call. = FALSE)
}

## ---- message=FALSE------------------------------------------------------
devtools::load_all() # in your script: library(recexcavAAR)
library(kriging)
library(magrittr)
library(plotly)

## ------------------------------------------------------------------------
edges <- data.frame(
  x = c(6.899, 10.658, 4.428, 0.669, 6.899, 10.658, 4.428, 0.669),
  y = c(19.292, 14.616, 9.597, 14.273, 19.292, 14.616, 9.597, 14.273),
  z = c(9.7, 9.7, 9.7, 9.7, 8.3, 8.3, 8.3, 8.3)
)

## ----fig.width=7, fig.height=5-------------------------------------------
rangex <- abs(max(edges$x) - min(edges$x))
rangey <- abs(max(edges$y) - min(edges$y))

vis <- plot_ly(
    data = rbind(
      edges[1:4, ], 
      edges[1, ], 
      edges[5:8, ], 
      edges[5, ],
      edges[c(6,2), ],
      edges[c(3,7), ],
      edges[c(8,4), ]
    ),
    x = x, y = y, z = z,
    mode = "lines", type = "scatter3d",
    line = list(size = 5, color = "black")
  ) %>% 
  layout(
    showlegend = FALSE,
    autorange = F, 
    aspectmode = 'manual', 
    scene = list(
      dragmode = "orbit",
      aspectratio = list(x = rangex, y = rangey, z = 5),
      camera = list(
        eye = list(x = -9, y = 9, z = 11) 
      )
    )
  ) 

vis

## ------------------------------------------------------------------------
sp <- KT_spits

splist <- list()
spitnames <- c("^surface", "^spit1", "^spit2", "^spit3", "^bottom")

for (i in 1:length(spitnames)){
  splist[[i]] <- sp[grep(spitnames[i], sp$id), ]
}

## ----fig.width=7, fig.height=5-------------------------------------------
# I had to choose a very low pixel value to keep the vignette small enough 
maps <- kriglist(splist, x = 2, y = 3, z = 4, lags = 3, model = "spherical", pixels = 20)

surf <- list()
for (i in 1:length(maps)) {
  surf[[i]] <- spatialwide(maps[[i]]$x, maps[[i]]$y, maps[[i]]$pred, digits = 3)
}

vis %>% 
  add_trace(
    x = surf[[1]]$x, y = surf[[1]]$y, z = surf[[1]]$z, type = "surface", 
    showscale = FALSE, opacity = 0.9, hoverinfo = "none"
  ) %>%
  add_trace(
    x = surf[[2]]$x, y = surf[[2]]$y, z = surf[[2]]$z, type = "surface", 
    showscale = FALSE, opacity = 0.9, hoverinfo = "none"
  ) %>%
  add_trace(
    x = surf[[3]]$x, y = surf[[3]]$y, z = surf[[3]]$z, type = "surface", 
    showscale = FALSE, opacity = 0.9, hoverinfo = "none"
  ) %>%
  add_trace(
    x = surf[[4]]$x, y = surf[[4]]$y, z = surf[[4]]$z, type = "surface", 
    showscale = FALSE, opacity = 0.9, hoverinfo = "none"
  ) %>%
  add_trace(
    x = surf[[5]]$x, y = surf[[5]]$y, z = surf[[5]]$z, type = "surface", 
    showscale = FALSE, opacity = 0.9, hoverinfo = "none"
  ) 

## ----fig.width=7, fig.height=5-------------------------------------------
for (i in 1:length(maps)) {
  rem <- recexcavAAR::pnpmulti(edges$x[1:4], edges$y[1:4], maps[[i]]$x, maps[[i]]$y)
  maps[[i]] <- maps[[i]][rem, ]
}

surf2 <- list()
for (i in 1:length(maps)) {
  surf2[[i]] <- recexcavAAR::spatialwide(maps[[i]]$x, maps[[i]]$y, maps[[i]]$pred, 3)
}

vis2 <- vis %>% 
  add_trace(
    x = surf2[[1]]$x, y = surf2[[1]]$y, z = surf2[[1]]$z, type = "surface", 
    showscale = FALSE, opacity = 0.9, hoverinfo = "none"
  ) %>%
  add_trace(
    x = surf2[[2]]$x, y = surf2[[2]]$y, z = surf2[[2]]$z, type = "surface", 
    showscale = FALSE, opacity = 0.9, hoverinfo = "none"
  ) %>%
  add_trace(
    x = surf2[[3]]$x, y = surf2[[3]]$y, z = surf2[[3]]$z, type = "surface", 
    showscale = FALSE, opacity = 0.9, hoverinfo = "none"
  ) %>%
  add_trace(
    x = surf2[[4]]$x, y = surf2[[4]]$y, z = surf2[[4]]$z, type = "surface", 
    showscale = FALSE, opacity = 0.9, hoverinfo = "none"
  ) %>%
  add_trace(
    x = surf2[[5]]$x, y = surf2[[5]]$y, z = surf2[[5]]$z, type = "surface", 
    showscale = FALSE, opacity = 0.9, hoverinfo = "none"
  ) 

vis2

## ----fig.width=7, fig.height=5-------------------------------------------
ve <- KT_vessel
vesselsingle <- ve[grep("KTF", ve$inv), ]

vis <- vis %>% 
  add_trace(
    data = vesselsingle, x = x, y = y, z = z, 
    mode = "markers", type = "scatter3d",
    marker = list(size = 3, color = "red"),
    hoverinfo = "text",
    text = paste(
      "SING: ", "inv = ", inv, "; spit = ", spit ,"; square = ", square, "; feature = ", feature,
      sep = ""
    )
  )

vis

## ------------------------------------------------------------------------
vesselmass <- ve[grep("KTM", ve$inv), ]

## ------------------------------------------------------------------------
sqc <- KT_squarecorners

squares <- list()
sqnum <- 1
for (i in 1:(nrow(sqc) - 9)) {
  if (i %% 9 == 0) {
    next
  } else {
    a <- sqc[i, ]
    b <- sqc[i + 1, ]
    c <- sqc[i + 9, ]
    d <- sqc[i + 10, ]
  }
  squares[[sqnum]] <- data.frame(
    x = c(a[, 1], b[, 1], c[, 1], d[, 1]), 
    y = c(a[, 2], b[, 2], c[, 2], d[, 2])
  )
  sqnum <- sqnum + 1
}

## ------------------------------------------------------------------------
sqcenters <- recexcavAAR::spitcenternatlist(squares, maps)

for (i in 1:length(sqcenters)) {
  sqcenters[[i]] <- data.frame(sqcenters[[i]], square = i, spit = c("spit1", "spit2", "spit3", "bottom"))
}

sqcdf <- do.call(rbind.data.frame, sqcenters)

## ----fig.width=7, fig.height=5-------------------------------------------
vis2 %>%
  add_trace(
    data = sqcdf, x = x, y = y, z = z,
    mode = "markers", type = "scatter3d",
    marker = list(size = 2, color = "green")
  )

## ----fig.width=7, fig.height=5, warning=FALSE----------------------------
vmsq <- merge(vesselmass[, 1:4], sqcdf, by = c("square", "spit"), all.x = TRUE)

vesselm <- vmsq[complete.cases(vmsq), ]

vis %>%
  add_trace(
    data = vesselm, x = x, y = y, z = z,
    mode = "markers", type = "scatter3d",
    marker = list(size = 3, color = "orange"),
    hoverinfo = "text",
    text = paste(
      "MASS: ", "inv = ", inv, "; spit = ", spit ,"; square = ", square, "; feature = ", feature,
      sep = ""
    )
  )

