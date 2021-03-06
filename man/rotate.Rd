% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{rotate}
\alias{rotate}
\title{Rotate a point cloud around a pivot point (3D)}
\usage{
rotate(x, y, z, degrx = 0, degry = 0, degrz = 0, pivotx = NA_real_,
  pivoty = NA_real_, pivotz = NA_real_)
}
\arguments{
\item{x}{vector of x axis values of rotation point cloud}

\item{y}{vector of y axis values of rotation point cloud}

\item{z}{vector of z axis values of rotation point cloud}

\item{degrx}{rotation angle around x axis in degree (default = 0)}

\item{degry}{rotation angle around y axis in degree (default = 0)}

\item{degrz}{rotation angle around z axis in degree (default = 0)}

\item{pivotx}{x axis value of pivot point (default = mean(x))}

\item{pivoty}{y axis value of pivot point (default = mean(y))}

\item{pivotz}{z axis value of pivot point (default = mean(z))}
}
\value{
data.frame with the spatial coordinates of the resulting points
}
\description{
Rotate a point cloud around a defined pivot point by defined angles. The default
rotation angle around each axis is zero and the default pivot point is the center
point of the point cloud (defined by mean())
}
\examples{
circ <- draw_circle(0,0,0,5)

#library(rgl)
#plot3d(
#  circ,
#  xlim = c(-6,6),
#  ylim = c(-6,6),
#  zlim = c(-6,6)
#)

rotcirc <- rotate(circ$x, circ$y, circ$z, degrx = 45)

#plot3d(
#  rotcirc,
#  xlim = c(-6,6),
#  ylim = c(-6,6),
#  zlim = c(-6,6)
#)

}
