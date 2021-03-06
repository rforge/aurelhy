# Construct an auremask object indicating regions to use for describing
# landscape around points in the DEM for AURELHY interpolation
# Note: dist are in km and angles in radians
"auremask" <- function (type = "radial", dist = c(1, 6, 11, 16, 21, 26),
angles = 0:7 * pi/4 + 0.01, n = 11, keep.origin = FALSE)
{	
	call <- match.call()
	keep.origin <- isTRUE(keep.origin)
	n <- round(n[1])
	# Make sure that n is odd
	if (n %% 2 == 0) n <- n + 1
	# Keep only largest dist for rect
	if (type == "rectangular") dist <- min(dist, na.rm = TRUE)	
		
	"radgrid" <- function (dist, angles, keep.origin) {
		grd <- data.frame(x = as.vector(dist %o% cos(angles)),
				   y = as.vector(dist %o% sin(angles)))
		# Do we have to add c(0, 0) to the list?
		if (keep.origin) {
			orig <- data.frame(x = 0, y = 0)
			return(rbind(orig, grd))
		} else return(grd)
	}
	
	"rectgrid" <- function (dist, n, keep.origin) {
		# The distances to consider
		dist <- (-n/2):(n/2) * dist
		grd <- data.frame(x = rep(dist, n), y = rep(dist, each = n))
		# Do we eliminate the origin?
		if (!keep.origin) {
			grd <- grd[grd$x != 0 | grd$y != 0, ]
		}
		return(grd)
	}
	
	# Type can be either "radial" or "rectangular"
	res <- switch(type,
		radial = radgrid(dist = dist, angles = angles, keep.origin = keep.origin),
		rectangular = rectgrid(dist = dist, n = n, keep.origin = keep.origin),
		stop("'type' must be \"radial\" or \"rectangular\""))
	# This is an 'auremask' object
	class(res) <- c("auremask", "data.frame")
	# Record parameters
	attr(res, "call") <- call
	attr(res, "type") <- type
	attr(res, "dist") <- dist
	attr(res, "angles") <- angles
	attr(res, "n") <- n
	attr(res, "keep.origin") <- keep.origin
	return(res)
}

# Print and plot methods for auremask objects
"print.auremask" <- function (x, geomat, ...)
{
	type <- attr(x, "type")
	cat("An auremask object defining a", type, "mask\n")
	orig.mes <-
		if (attr(x, "keep.origin")) "including origin" else "excluding origin"
	if (type == "radial") {
		cat("The window of analysis uses", nrow(x), "points", orig.mes, "\n")
		cat("Distance considered (km):\n")
		print(attr(x, "dist"))
		cat("... at angles (rad):\n")
		print(round(attr(x, "angles"), digits = 3))
	} else {
		n <- attr(x, "n")
		ntot <- n*n
		if (!attr(x, "keep.origin")) ntot <- ntot - 1
		cat("The window of analysis uses", ntot, "points", orig.mes, "\n")
		cat("The window uses", n, "distances spaced each by",
			attr(x, "dist"), "km\n")
	}
	# If we provide a geomat, look at how many points are in each sector
	if (!missing(geomat)) {
		if (!inherits(geomat, "geomat"))
			stop("'geomat' must be a geomat object")
		# We choose a point in the middle of the grid
		coords <- coords(geomat)
		x0 <- coords(geomat, "x")[nrow(geomat) %/% 2]
		y0 <- coords(geomat, "y")[ncol(geomat) %/% 2]
		maxdist <- max(attr(x, "dist"))
		if (type == "rectangular") maxdist <- maxdist * (attr(x, "n") / 2)
		# Calculation of the size of ane degree in latitude/longitude according to
		# central latitude in the considered geographical area
		meanlat <- mean(range(coords(geomat, type = "y")))
		lenx <- deg.lon(meanlat)
		maxdegx <- maxdist / lenx
		leny <- deg.lat(meanlat)
		maxdegy <- maxdist / leny
		mx <- maxdegx * 1.05
		my <- maxdegx * 1.05
		xlim <- c(x0 - mx, x0 + mx)
		ylim <- c(y0 - my, y0 + my)
		# Take a window out of these data
		geomat2 <- window(geomat, xlim, ylim)
		if (type == "radial") {			
			# Get the different groups to be used in different colors
			pc <- polar.coords(geomat2, x0, y0, maxdist)
			# Make classes for angles and distances
			dists <- attr(x, "dist")
			angles <- attr(x, "angles")
			pc$dist <- cut(pc$dist, breaks = dists,  labels = 1:(length(dists) - 1))
			pc$angle <- cut(pc$angle, breaks = c(angles, 8),  labels = 1:length(angles))
			pc <- pc[!is.na(pc$dist) & !is.na(pc$angle), ]
			cat("Total number of points used:", NROW(pc), "\n")
			cat("with the following repartition per sector:\n")
			# Print a contingency table
			print(table(dist = pc$dist, angle = pc$angle))
		} else {
			# Select rectangular grid sectors and look which points are in each
			# rectangle in the geomat's grid
			pt <- coords(geomat2, "xy")
			meanlat2 <- mean(range(coords(geomat2, type = "y")))
			xcut <- unique(x$x) / deg.lon(meanlat2) + x0
			ycut <- unique(x$y) / deg.lat(meanlat2) + y0
			pt$x <- cut(pt$x, breaks = xcut,  labels = 1:(length(xcut) - 1))
			pt$y <- cut(pt$y, breaks = ycut,  labels = 1:(length(ycut) - 1))
			# Eliminate data at origin, in case keep.origin == FALSE
			if (!attr(x, "keep.origin"))
				pt$x[pt$x == (length(xcut) - 1 ) %/% 2 + 1 &
					 pt$y == (length(ycut) - 1 ) %/% 2 + 1] <- NA
			pt <- pt[!is.na(pt$x) & !is.na(pt$y), ]
			cat("Total number of points used:", NROW(pt), "\n")
			cat("with the following repartition per sector:\n")
			# Print a contingency table
			print(table(x = pt$x, y = pt$y))
		}
	}
	return(invisible(x))
}

# Plot a mask. If y is provided, it must be a geomat object and the function
# tries to match distances with grid points and displays the result in the graph
"plot.auremask" <- function (x, y, ...)
{
	plot(x$x, x$y, xlab = "distance (km)", ylab = "distance (km)", asp = 1, type = "n", ...)
	type <- attr(x, "type")
	if (type == "radial") {
		radline <- function (angle, max) {
			lines(c(-max, max) * cos(angle), c(-max, max) * sin(angle), col = "gray")
		}
		maxx <- max(x$x)
		angles <- attr(x, "angles") 
		for (angle in angles) radline(angle, maxx)
		circle <- function (r) {
			# We choose a resolution of 50 points
			ang <- 0:50/25 * pi
			x <- r * cos(ang)
			y <- r * sin(ang)
			lines(x, y, col = "gray")
		}
		dists <- attr(x, "dist")
		for (dist in dists) circle(dist)
	} else { # Rectangular
		segments(x0 = min(x$x), y0 = unique(x$y), x1 = max(x$x), col = "gray")
		segments(x0 = unique(x$x), y0 = min(x$y), y1 = max(x$y), col = "gray")
	}
	# Do we match data to grid?
	if (missing(y)) return(invisible(x))
	if (!inherits(y, "geomat"))
		stop("'y' must be a geomat object")
	# we choose a point in the middle of the grid
	coords <- coords(y)
	x0 <- coords(y, "x")[nrow(y) %/% 2]
	y0 <- coords(y, "y")[ncol(y) %/% 2]
	maxdist <- max(attr(x, "dist"))
	if (type == "rectangular") maxdist <- maxdist * (attr(x, "n") / 2)
	# Calculation of the size of ane degree in latitude/longitude according to
	# central latitude in the considered geographical area
	meanlat <- mean(range(coords(y, type = "y")))
	lenx <- deg.lon(meanlat)
	maxdegx <- maxdist / lenx
	leny <- deg.lat(meanlat)
	maxdegy <- maxdist / leny
	mx <- maxdegx * 1.05
	my <- maxdegy * 1.05
	xlim <- c(x0 - mx, x0 + mx)
	ylim <- c(y0 - my, y0 + my)
	# Take a window out of these data
	geomat <- window(y, xlim, ylim)
	pt <- coords(geomat, "xy")
	meanlat2 <- mean(range(coords(geomat, type = "y")))
	lenx2 <- deg.lon(meanlat2)
	leny2 <- deg.lat(meanlat2)
	if (type == "radial") {
		# Get the different groups to be used in different colors
		pc <- polar.coords(geomat, x0, y0, maxdist)
		# Make classes for angles and distances
		pc$dist <- cut(pc$dist, breaks = c(0, dists),  labels = 0:(length(dists) - 1))
		pc$angle <- cut(pc$angle, breaks = c(angles, 8),  labels = 1:length(angles))
		# Use four different colors
		cols <- (2 * as.numeric(pc$dist) %% 2) + (as.numeric(pc$angle) %% 2) + 1
		points((pt$x - x0) * lenx2, (pt$y - y0) * leny2, pch = "+", cex = 0.5, col = cols)
	} else { # Rectangular data
		# Select rectangular grid sectors and look which points are in each
		# rectangle in the geomat's grid
		xcut <- unique(x$x) / lenx2 + x0
		ycut <- unique(x$y) / leny2 + y0
		pc <- pt
		pc$x <- cut(pc$x, breaks = xcut,  labels = 1:(length(xcut) - 1))
		pc$y <- cut(pc$y, breaks = ycut,  labels = 1:(length(ycut) - 1))
		# Eliminate data at origin, in case keep.origin == FALSE
		if (!attr(x, "keep.origin"))
			pc$x[pc$x == (length(xcut) - 1 ) %/% 2 + 1 &
				 pc$y == (length(ycut) - 1 ) %/% 2 + 1] <- NA

		# Use fours different colors
		cols <- (2 * as.numeric(pc$x) %% 2) + (as.numeric(pc$y) %% 2) + 1
		points((pt$x - x0) * lenx2, (pt$y - y0) * leny2, pch = "+", cex = 0.5, col = cols)	
	}	
}
