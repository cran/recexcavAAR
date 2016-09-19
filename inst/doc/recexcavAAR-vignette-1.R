## ---- echo=FALSE---------------------------------------------------------
if (!requireNamespace("plotly", quietly = TRUE)) {
  stop("plotly needed for this vignette to build.",
    call. = FALSE)
}

## ---- message=FALSE------------------------------------------------------
devtools::load_all() # in your script: library(recexcavAAR)
library(dplyr)
library(kriging)
library(magrittr)
library(plotly)

## ------------------------------------------------------------------------
edges <- data.frame(
  x = c(0, 3, 0, 3, 0, 3, 0, 3),
  y = c(0, 0, 0, 0, 1, 1, 1, 1),
  z = c(0, 0, 2, 2, 0, 0, 2, 2)
)

## ----fig.width=7, fig.height=5-------------------------------------------
vis <- plot_ly(
  edges, x = x, y = y, z = z, type = "scatter3d", mode = "markers"
  ) %>% 
  layout(
    showlegend = FALSE,
    autorange = F, 
    aspectmode = 'manual', 
    scene = list(
      dragmode = "orbit",
      aspectratio = list(x=3, y=1, z=3),
      camera = list(
        eye = list(x = 4, y = 4, z = 1) 
      )
    )
  )

vis

## ------------------------------------------------------------------------
df1 <- data.frame(
  x = c(rep(0, 6), seq(0.2, 2.8, 0.2), seq(0.2, 2.8, 0.2), rep(3,6)),
  y = c(seq(0, 1, 0.2), rep(0, 14), rep(1, 14), seq(0, 1, 0.2)), 
  z = c(0.9+0.05*rnorm(6), 0.9+0.05*rnorm(14), 1.3+0.05*rnorm(14), 1.2+0.05*rnorm(6))
)

df2 <- data.frame(
  x = c(rep(0, 6), seq(0.2, 2.8, 0.2), seq(0.2, 2.8, 0.2), rep(3,6)),
  y = c(seq(0, 1, 0.2), rep(0, 14), rep(1, 14), seq(0, 1, 0.2)),
  z = c(0.6+0.05*rnorm(6), 0.6+0.05*rnorm(14), 1.0+0.05*rnorm(14), 0.9+0.05*rnorm(6))
)

## ----fig.width=7, fig.height=5-------------------------------------------
vis <- vis %>%
  add_trace(
    data = df1, x = x, y = y, z = z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 4, color = "green", symbol = 104)
  ) %>%
  add_trace(
    data = df2, x = x, y = y, z = z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 4, color = "blue", symbol = 104)
  ) 

vis

## ------------------------------------------------------------------------
lpoints <- list(df1, df2)

maps <- kriglist(lpoints, lags = 3, model = "spherical", pixels = 30)

## ------------------------------------------------------------------------
surf1 <- spatialwide(maps[[1]]$x, maps[[1]]$y, maps[[1]]$pred, 3)
surf2 <- spatialwide(maps[[2]]$x, maps[[2]]$y, maps[[2]]$pred, 3)

## ----fig.width=7, fig.height=5-------------------------------------------
vis <- vis %>% 
  add_trace(
    x = surf1$x, y = surf1$y, z = surf1$z, type = "surface", showscale = FALSE
  ) %>%
  add_trace(
    x = surf2$x, y = surf2$y, z = surf2$z, type = "surface", showscale = FALSE
  ) 

vis

## ----fig.width=7, fig.height=5-------------------------------------------
hexatestdf <- data.frame(
  x = c(1, 1, 1, 1, 2, 2, 2, 2),
  y = c(0, 1, 0, 1, 0, 1, 0, 1),
  z = c(0.8, 0.8, 1, 1, 0.8, 0.8, 1, 1)
)

## ------------------------------------------------------------------------
cx = fillhexa(hexatestdf, 0.05)

## ----fig.width=7, fig.height=5-------------------------------------------
vis %>% 
  add_trace(
    data = cx, x = x, y = y, z = z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "red", symbol = 104)
  )

## ----fig.width=7, fig.height=5-------------------------------------------
cuberasterlist <- list(cx)

crlist <- posdeclist(cuberasterlist, maps)

hexa <- crlist[[1]]

a <- filter(
  hexa,
  pos == 0
)

b <- filter(
  hexa,
  pos == 1
)

c <- filter(
  hexa,
  pos == 2
)

vis %>% 
  add_trace(
    data = a, x = x, y = y, z = z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#31a354", symbol = 104)
  ) %>% 
  add_trace(
    data = b, x = x, y = y, z = z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#de2d26", symbol = 104)
  ) %>% 
  add_trace(
    data = c, x = x, y = y, z = z, 
    mode = "markers", type = "scatter3d", 
    marker = list(size = 1, color = "#2b8cbe", symbol = 104)
  )

## ------------------------------------------------------------------------
sapply(
  crlist, 
  function(x){
    x$pos %>%
    table() %>%
    prop.table() %>%
    multiply_by(100) %>%
    round(2)
  }
  ) %>% t

