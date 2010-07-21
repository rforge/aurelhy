# Copyright (c) 2010, Ph. Grosjean <phgrosjean@sciviews.org>
#
# This file is part of agromet
#
# agromet is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# agromet is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with agromet. If not, see <http://www.gnu.org/licenses/>.

"AMDlg" <- function ()
{
	# If the window is already created, just activate it...
	if ("AMDlgWin" %in% WinNames()) {
		AMDlgWin <- WinGet("AMDlgWin")
		tkfocus(AMDlgWin)  	# Doesn't work with Rgui.exe, but next command does
		tkwm.deiconify(AMDlgWin)
    	return(invisible())
	}

	# Construct the window
	tkWinAdd("AMDlgWin", title = paste(getTemp("AMname"), "assistant"),
		pos = "-100+10")
	AMDlgWin <- WinGet("AMDlgWin")

	# Do not show it until it is completelly constructed!
	tkwm.withdraw(AMDlgWin)
	on.exit(tkwm.deiconify(AMDlgWin))

	# Change the icon of that window (if under Windows)
	if (isWin()) tk2ico.set(AMDlgWin, getTemp("AMico"))

	# Add a menu (load it from a spec file)
	Pkg <- getTemp("AMguiPackage", default = "agromet")
	MenuReadPackage(Pkg, file = "MenusAMDlgWin.txt")

	# Add a toolbar (read it from file 'ToolbarAMDlgWin.txt')
	ToolRead(file.path(getTemp("AMgui"), "ToolbarsAMDlgWin.txt"))
	# Add tooltip text
	.guiTools <- getTemp(".guiTools")
	N <- names(.guiTools)
	for (i in 2:length(N)) {
		tip <- basename(N[i])
		if (grepl("^[^-]", tip))
			tk2tip(.guiTools[[N[i]]], tip)
	}

	# Add a statusbar with a text and a progressbar
	status <- tk2frame(AMDlgWin)
	statusText <- tk2label(status, text = paste("Ready -", getwd()),
		justify = "left", anchor = "w", width = 60)
	statusProg <- tk2progress(status, orient = "horizontal", maximum = 100)
	tkpack(statusProg, side = "right")
	tkpack(statusText, side = "left", fill= "x")
	tkpack(status, side = "bottom", fill = "x")
	tkpack(tk2separator(AMDlgWin), side = "bottom", fill = "x")

	# Keep track of statusText / statusProg
	assignTemp("statusText", statusText)
	assignTemp("statusProg", statusProg)
	## Change value of the progressbar
	#tkconfigure(getTemp("statusProg"), value = 50)
	## Change text of the statusbar
	#tkconfigure(getTemp("statusText"), text = paste("Ready -", getwd()))

	if (!isWin()) {
		# The activate R console & R graph do not work elsewhere
        MenuStateItem("$Tk.AMDlgWin/Apps", "&R Console", FALSE)
		MenuStateItem("$Tk.AMDlgWin/Apps", "Active R &Graph", FALSE)
	}

	# Change the window to non resizable and topmost (f under Windows)
	if (isWin()) tcl("wm", "attributes", AMDlgWin, topmost = 1)
	tkwm.resizable(AMDlgWin, 0, 0)
	# Focus on that window
	tkfocus(AMDlgWin)	# Doesn't work with Rgui.exe, but tkwm.deiconify does
}

"aboutAgromet" <- function (graphical = FALSE)
{
	msg <- getTemp("AMverstring")
	if (isTRUE(graphical)) {
		tkmessageBox(message = msg, title = "About...", icon = "info",
			type = "ok")
	} else cat(msg, "\n")
}

"exitAgromet" <- function ()
{
	detach("package:agromet")
	cat("agromet package unloaded; To restart it, issue:\n> library(agromet)\n")
}

"closeAgromet" <- function ()
{
	closeAgrometAssistant()
	exitAgromet()
}

"closeAgrometAssistant" <- function ()
	tkWinDel("AMDlgWin")

"focusR" <- function ()
{
	# Switch the focus to the R console
	# This command is only available with Rgui
	if (isRgui()) bringToTop(-1)
}

"focusGraph" <- function ()
{
	# Focus to the active R graph (create one if there is no graph device)
	# This command is only available with Rgui
	if (is.null(dev.list())) {
		device <- match.fun(getOption("device"))
		device()
	} else {
		# Activate current graph window
		if (isRgui()) bringToTop()
	}
}

"importAM" <- function ()
{
	file <- paste(as.character(tkgetOpenFile(filetypes = "{{ESRI file} {.asc .shp}}",
			title = "Import data...")), collapse = " ")
	if (length(file) == 0 || file == "") return(invisible())
	# Importation depends if we have a grid or shape file
	if (grepl("[.][aA][sS][cC]$", file)) {
		# Ask for the kind of object to create
		kind <- select.list(c("geomat (general numerical grid)",
			"geotm (terrain model, integers)",
			"geomask (mask, booleans)"),
			title = "AgroMet object to create...", graphics = TRUE)
		if (length(kind) == 0) return(invisible())
		# Ask for the name of the object
		obj <- guiDlgInput("Name of the object (use only a-zA-Z0-9._):",
			paste("Import data into a", kind),
			make.names(sub("[.][aA][sS][cC]$", "", basename(file))))
		if (length(obj) == 0) return(invisible()) else obj <- make.names(obj)
		if (exists(obj, envir = .GlobalEnv)) {
			res <- guiDlgMessage(paste("'", obj,
				"' already exists. Do you want to replace it with the imported data?", sep = ""),
				"Existing object", "yesno", icon = "question")
			if (res == "no") return(invisible())
		}
		# Import the data now
		if (kind == "geomat (general numerical grid)") {
			assign(obj, read.geomat(file), envir = .GlobalEnv)
		} else if (kind == "geotm (terrain model, integers)") {
			assign(obj, read.geotm(file), envir = .GlobalEnv)
		} else if (kind == "geomask (mask, booleans)") {
			assign(obj, read.geomask(file), envir = .GlobalEnv)
		}
	
	} else { # This must be an ESRI SHAPE file (.shp)
		# Ask for the kind of object to create
		kind <- select.list(c("geoshapes (one or more shapes)",
			"geopoints (georeferenced points)"),
			title = "AgroMet object to create...", graphics = TRUE)
		if (length(kind) == 0) return(invisible())
		# Ask for the name of the object
		obj <- guiDlgInput("Name of the object (use only a-zA-Z0-9._):",
			paste("Import data into a", kind),
			make.names(sub("[.][sS][hH][pP]$", "", basename(file))))
		if (length(obj) == 0) return(invisible()) else obj <- make.names(obj)
		if (exists(obj, envir = .GlobalEnv)) {
			res <- guiDlgMessage(paste("'", obj,
				"' already exists. Do you want to replace it with the imported data?", sep = ""),
				"Existing object", "yesno", icon = "question")
			if (res == "no") return(invisible())
		}
		# Import the data now
		if (kind == "geoshapes (one or more shapes)") {
			assign(obj, read.geoshapes(file), envir = .GlobalEnv)
		} else if (kind == "geopoints (georeferenced points)") {
			assign(obj, read.geopoints(file), envir = .GlobalEnv)
		}
	}
}

"exportAM" <- function ()
{
	Object <- getVar(c("geomat", "geoshapes", "geopoints",
		"predict.aurelhy"), multi = TRUE, title = paste("Choose one ", getTemp("AMname"),
		"object:"), warn.only = FALSE)
	if (length(Object) == 0 || (length(Object) == 1 && Object == ""))
		return(invisible())
	# Depending on which object is choosen, we propose to save it the appropriate way
	obj <- get(Object, envir = .GlobalEnv)
	if (inherits(obj, "predict.aurelhy")) {
		# Export the result transformed into a geomat
		obj <- as.geomat(obj)
	}
	
	if (inherits(obj, "geotm")) {
		file <- paste(as.character(tkgetSaveFile(filetypes = "{{ESRI ASCII grid} {.asc}}",
			initialfile = paste(Object, ".asc", sep = ""),
			title = paste("Export", Object, "..."))),
			collapse = " ")
		if (length(file) == 0 || file == "") return(invisible())
		if (regexpr("[.][aA][sS][cC]$", file) < 0)
			file <- paste(file, ".asc", sep = "")
		write.geotm(obj, file = file)

	} else if (inherits(obj, "geomask")) {
		file <- paste(as.character(tkgetSaveFile(filetypes = "{{ESRI ASCII grid} {.asc}}",
			initialfile = paste(Object, ".asc", sep = ""),
			title = paste("Export", Object, "..."))),
			collapse = " ")
		if (length(file) == 0 || file == "") return(invisible())
		if (regexpr("[.][aA][sS][cC]$", file) < 0)
			file <- paste(file, ".asc", sep = "")
		write.geomask(obj, file = file)

	} else if (inherits(obj, "geomat")) {
		file <- paste(as.character(tkgetSaveFile(filetypes = "{{ESRI ASCII grid} {.asc}}",
			initialfile = paste(Object, ".asc", sep = ""),
			title = paste("Export", Object, "..."))),
			collapse = " ")
		if (length(file) == 0 || file == "") return(invisible())
		if (regexpr("[.][aA][sS][cC]$", file) < 0)
			file <- paste(file, ".asc", sep = "")
		write.geomat(obj, file = file)		

	} else if (inherits(obj, "geoshapes")) {
		file <- paste(as.character(tkgetSaveFile(filetypes = "{{ESRI SHAPE file} {.shp}}",
			initialfile = paste(Object, ".shp", sep = ""),
			title = paste("Export", Object, "..."))),
			collapse = " ")
		if (length(file) == 0 || file == "") return(invisible())
		# Eliminate extension, if provided as .shp, shx, or .dbf
		file <- sub("[.][sSdD][hHbB][pPxXfF]$", "", file)
		write.geoshapes(obj, file = file)			

	} else if (inherits(obj, "geopoints")) {
		file <- paste(as.character(tkgetSaveFile(filetypes = "{{ESRI SHAPE file} {.shp}}",
			initialfile = paste(Object, ".shp", sep = ""),
			title = paste("Export", Object, "..."))),
			collapse = " ")
		if (length(file) == 0 || file == "") return(invisible())
		# Eliminate extension, if provided as .shp, shx, or .dbf
		file <- sub("[.][sSdD][hHbB][pPxXfF]$", "", file)
		write.geopoints(obj, file = file)
		
	} else stop("Unrecognized object class")
}

"loadObjects" <- function ()
{
	file <- file.choose()
	# The following code is better, but works only on Windows
	#file <- selectFile("RData", multi = FALSE, quote = FALSE,
	#	title = "Select a RData file...")
	if ( is.null(file) || length(file) == 0 || file == "")
		return(invisible()) # Cancelled dialog box
	if (file.exists(file))
		load(file, envir = .GlobalEnv)
}

"saveObjects" <- function ()
{
	Objects <- getVar(c("geomat", "geoshapes", "geopoints", "auremask",
		"aurelhy", "predict.aurelhy"), multi = TRUE, title = paste("Choose", getTemp("AMname"),
		"object(s):"), warn.only = FALSE)
	if (length(Objects) == 0 || (length(Objects) == 1 && Objects == ""))
		return(invisible())
	file <- paste(as.character(tkgetSaveFile(filetypes = "{{R data} {.RData}}",
			initialfile = paste(getTemp("AMname"), ".RData", sep = ""),
			title = paste("Save", getTemp("AMname"), "data under..."))),
			collapse = " ")
	if (length(file) == 0 || file == "") return(invisible())
	if (regexpr("[.][rR][dD][aA][tT][aA]$", file) < 0)
		file <- paste(file, ".RData", sep = "")
	save(list = Objects, file = file, compress = TRUE)
}

"listObjects" <- function ()
{
    varlist <- objects(pos = 1)
	if (length(varlist) == 0)
		stop("No objects currently loaded in memory!\n")
	Filter <- NULL
	for (i in 1:length(varlist)) Filter[i] <- inherits(get(varlist[i]),
		c("geomat", "geoshapes", "geopoints", "auremask", "aurelhy", "predict.aurelhy"))
	varlist <- varlist[Filter]
	if (length(varlist) == 0) {
		stop("No ", getTemp("AMname"), " objects currently loaded in memory!\n")
	} else {
    	print(varlist)
	}
}

"removeObjects" <- function ()
{
	Objects <- getVar(c("geomat", "geoshapes", "geopoints", "auremask",
		"aurelhy", "predict.aurelhy"), multi = TRUE,
	title = paste(getTemp("AMname"), "object(s) to remove:"), warn.only = FALSE)
	if (length(Objects) == 0 || (length(Objects) == 1 && Objects == ""))
		return(invisible())
	rm(list = Objects, envir = .GlobalEnv)
}

"printObject" <- function ()
{
	Obj <- getVar(c("geomat", "geoshapes", "geopoints", "auremask",
		"aurelhy", "predict.aurelhy"), multi = FALSE,
	title = paste(getTemp("AMname"), "object(s) to remove:"), warn.only = FALSE)
	if (length(Obj) == 0 || (length(Obj) == 1 && Obj == ""))
		return(invisible())
	Obj <- get(Obj, envir = .GlobalEnv)
	print(Obj)
}

"getVar" <- function (class = "data.frame", default = "", multi = FALSE,
title = paste("Choose a ", class, ":", sep = ""), warn.only = TRUE)
{	
	# Get one or several variables of a given object class
	varlist <- objects(pos = 1)	# Get objects in .GlobalEnv
	if (length(varlist) == 0) {
		msg <- paste("There is no object of class '",
			paste(class, collapse = " "), "' in the user workspace!", sep = "")
		if (isTRUE(warn.only)) warning(msg) else stop(msg)
		return("")
	}
	# Filter this list to keep only object inheriting a giving class...
	Filter <- NULL
	for (i in 1:length(varlist))
		Filter[i] <- inherits(get(varlist[i]), class)
	
	# Keep only those objects
	varlist <- varlist[Filter]	
	if (length(varlist) == 0) {	# No such objects in .GlobalEnv
		msg <- paste("There is no object of class '",
			paste(class, collapse = " "), "' in the user workspace!", sep = "")
		if (isTRUE(warn.only)) warning(msg) else stop(msg)
		varsel <- "" 
	} else {
		if (default == "") default <- varlist[1]
		varsel <- select.list(varlist, preselect = default, multiple = multi,
			title = title)
	}
    return(varsel)		
}

"viewAuremask" <- function ()
{
	# Load an auremask, then a geotm and print()/plot()
	Amask <- getVar("auremask", warn.only = FALSE)
	if (length(Amask) == 0 || (length(Amask) == 1 && Amask == ""))
		return(invisible())
	TM <- getVar("geotm", warn.only = FALSE)
	if (length(TM) == 0 || (length(TM) == 1 && TM == ""))
		return(invisible())
	Amask <- get(Amask, envir = .GlobalEnv)
	TM <- get(TM, envir = .GlobalEnv)
	print(Amask, TM)
	plot(Amask, TM)
}

"viewAurelhy" <- function ()
{
	# Select an aurelhy object and display the PCA screeplot + summary
	A <- getVar("aurelhy", warn.only = FALSE)
	if (length(A) == 0 || (length(A) == 1 && A == ""))
		return(invisible())
	A <- get(A, envir = .GlobalEnv)
	print(summary(A))
	plot(A)
}

"viewAurelhyPredict" <- function ()
{
	# Select an aurelhy object and display the regression/krige plot + summary
	Ap <- getVar("predict.aurelhy", warn.only = FALSE)
	if (length(Ap) == 0 || (length(Ap) == 1 && Ap == ""))
		return(invisible())
	Ap <- get(Ap, envir = .GlobalEnv)
	# Select the plot(s) to create...
	PlList <- c("Residuals versus fitted", "Normal Q-Q plot of residuals",
				"Residuals heteroskedasticity", "Cook's distance of residuals",
				"Residuals leverage (influencial points)",
				"Model adjusted on the semi-variogram",
				"Model prediction and kriged residuals")
	Pl <- select.list(PlList, preselect = "Model prediction and kriged residuals",
		multiple = TRUE, title = "Select one or more graphs", graphics = TRUE)
	if (length(Pl) < 1) return(invisible())
	print(summary(Ap))
	if ("Residuals versus fitted" %in% Pl) plot(Ap, which = 1)
	if ("Normal Q-Q plot of residuals" %in% Pl) plot(Ap, which = 2)
	if ("Residuals heteroskedasticity" %in% Pl) plot(Ap, which = 3)
	if ("Cook's distance of residuals" %in% Pl) plot(Ap, which = 4)
	if ("Residuals leverage (influencial points)" %in% Pl) plot(Ap, which = 5)
	if ("Model adjusted on the semi-variogram" %in% Pl) plot(Ap, which = 6)
	if ("Model prediction and kriged residuals" %in% Pl) plot(Ap, which = 7)
}

"updatePca" <- function ()
{
	# Select nbr.pc and scale and update the aurelhy object
	A <- getVar("aurelhy", warn.only = FALSE)
	if (length(A) == 0 || (length(A) == 1 && A == ""))
		return(invisible())
	obj <- get(A, envir = .GlobalEnv)
	# Get the number of PCs to keep
	npc <- attr(obj, "nbr.pc")
	npc2 <- guiDlgInput("Number of PCs to keep for landscape descriptors:",
		"Update PCA", npc)
	if (length(npc2) == 0) return(invisible())
	npc2 <- as.integer(npc2)[1]
	if (npc2 < 1) stop("You cannot use less than 1 PC")
	npcmax <- ncol(attr(obj, "land"))
	if (npc2 > npcmax) stop("You cannot use more than ", npcmax, "PCs")
		
	# Do we scale the variables?
	res <- guiDlgMessage("Do you want to scale initial land descriptors (same variance)? [no in original AURELHY method]",
		"Scale PCA", "yesno", icon = "question")
	if (res == "no") scale <- FALSE else scale <- TRUE
	
	# Update the object
	assign(A, update(obj, nbr.pc = npc2, scale = scale),envir = .GlobalEnv)
}

"updateLm" <- function ()
{
	# Select an aurelhy object and update its regression model
	A <- getVar("aurelhy", warn.only = FALSE)
	if (length(A) == 0 || (length(A) == 1 && A == ""))
		return(invisible())
	obj <- get(A, envir = .GlobalEnv)
	mod <- deparse(attr(obj, "model"))
	# Enter a regression model and update it in the object
	mod2 <- guiDlgInput(paste("Model to use for the regression with this aurelhy object\n(variables are: ", paste(names(obj), collapse = ", "), ")", sep = ""),
		"Update regression model", mod)
	if (length(mod2) == 0 || mod2 == mod) return(invisible())
	assign(A, update(obj, model = mod2), envir = .GlobalEnv)
}

"updateVgm" <- function ()
{
	# Enter code for calculating a vgm() and update it in the object
	A <- getVar("aurelhy", warn.only = FALSE)
	if (length(A) == 0 || (length(A) == 1 && A == ""))
		return(invisible())
	obj <- get(A, envir = .GlobalEnv)
	vgm <- 'vgm(1, "Sph", 10, 1)'
	# Enter a variogram model and update it in the object
	vgm2 <- guiDlgInput('Variogram model to use with this aurelhy object\ne.g., vgm(1, "Sph", 10, 0.5): psill, model as Sph, Exp, Gau, etc., range and nugget',
		"Update variogram model", vgm)
	if (length(vgm2) == 0 || vgm2 == vgm) return(invisible())
	assign(A, update(obj, vgmodel = eval(parse(text = vgm2))), envir = .GlobalEnv)
}

"interpolateAurelhy" <- function ()
{
	# This function performs interpolation of some data, using an aurelhy object
	# The aurelhy object
	A <- getVar("aurelhy", warn.only = FALSE)
	if (length(A) == 0 || (length(A) == 1 && A == ""))
		return(invisible())
	myaurelhy <- get(A, envir = .GlobalEnv)
	
	# The geopoints object
	P <- getVar("geopoints", warn.only = FALSE)
	if (length(P) == 0 || (length(P) == 1 && P == ""))
		return(invisible())
	pts <- get(P, envir = .GlobalEnv)
	# Get the list of variables available in pts
	vars <- names(pts)
	# Eliminate 'Id', 'x' and 'y'
	vars <- vars[!vars %in% c("Id", "x", "y")]
	myvar <- select.list(vars, multiple = FALSE,
		title = "Select one variable to interpolate", graphics = TRUE)
	if (length(myvar) == 0) return(invisible())	
	# Indicate the name of the resulting predict.aurelhy object
	obj <- guiDlgInput("Name of the object (use only a-zA-Z0-9._):",
		"Interpolate data with AURELHY",
		paste(P, myvar, sep = "."))
	if (length(obj) == 0) return(invisible()) else obj <- make.names(obj)
	if (exists(obj, envir = .GlobalEnv)) {
		res <- guiDlgMessage(paste("'", obj,
			"' already exists. Do you want to replace it?", sep = ""),
			"Existing object", "yesno", icon = "question")
		if (res == "no") return(invisible())
	}
	assign(obj, predict(myaurelhy, pts, myvar), envir = .GlobalEnv)
}

"extractPca" <- function ()
{
	# Extract one PCA component and format it as a geomat object
	A <- getVar("aurelhy", warn.only = FALSE)
	if (length(A) == 0 || (length(A) == 1 && A == ""))
		return(invisible())
	obj <- get(A, envir = .GlobalEnv)
	# Determine which PC to extract
	npc <- attr(obj, "nbr.pc")
	pcs <- paste("PC", 1:npc, sep = "")
	pc <- select.list(pcs, multiple = FALSE,
		title = "Select one PC to extract", graphics = TRUE)
	if (length(pc) == 0) return(invisible())	
	objn <- paste(A, pc, sep = ".")
	assign(objn, as.geomat(obj, pc), envir = .GlobalEnv)
	cat(pc, " extracted as ", objn, "\n", sep = "")
}

"extractInterpolated" <- function ()
{
	# Extract interpolated data and convert to a geomat
	A <- getVar("predict.aurelhy", warn.only = FALSE)
	if (length(A) == 0 || (length(A) == 1 && A == ""))
		return(invisible())
	obj <- get(A, envir = .GlobalEnv)
	# Choose the name for interpolated data
	objn <- guiDlgInput("Name of the geomat object with your interpolated data:",
		"Extract interpolated data", paste(A, "interpolated", sep = "."))
	if (length(objn) == 0) return(invisible())
	# Check if this object already exists
	if (exists(objn, envir = .GlobalEnv)) {
		res <- guiDlgMessage(paste("'", objn,
			"' already exists. Do you want to replace it?", sep = ""),
			"Existing object", "yesno", icon = "question")
		if (res == "no") return(invisible())
	}
	assign(objn, as.geomat(obj), envir = .GlobalEnv)
}

# This is just a wrapper function for image.geomat
# to be called by the GUI with guiDlgFunction("map")
"map" <- function (x, max.xgrid = 500, col = terrain.colors(50), add = FALSE, 
xlab = if (add) "" else "Longitude", ylab = if (add) "" else "Latitude", 
asp = 1, ...) {
	aurelhy:::image.geomat(x = x, max.xgrid = max.xgrid, col = col, add = add,
		xlab = xlab, ylab = ylab, asp = asp, ...)
}