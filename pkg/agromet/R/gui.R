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
	### TODO: notify this command is not available elsewhere (inactivate menu?)
	if (isRgui()) bringToTop(-1)
}

"focusGraph" <- function ()
{
	# Focus to the active R graph (create one if there is no graph device)
	### TODO: notify this command is not available elsewhere (inactivate menu?)
	if (is.null(dev.list())) {
		device <- match.fun(getOption("device"))
		device()
	} else {
		# Activate current graph window
		if (isRgui()) bringToTop()
	}
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
		"aurelhy"), multi = TRUE, title = paste("Choose", getTemp("AMname"),
		"object(s):"), warn.only = FALSE)
	if (length(Objects) == 0 || (length(Objects) == 1 && Objects == ""))
		return(invisible())
	file <- paste(as.character(tkgetSaveFile(filetypes = "{{R data} {.RData}}",
			initialfile = paste(getTemp("ZIname"), ".RData", sep = ""),
			title = paste("Save", getTemp("ZIname"), "data under..."))),
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
		c("geomat", "geoshapes", "geopoints", "auremask", "aurelhy"))
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
		"aurelhy"), multi = TRUE,
	title = paste(getTemp("AMname"), "object(s) to remove:"), warn.only = FALSE)
	if (length(Objects) == 0 || (length(Objects) == 1 && Objects == ""))
		return(invisible())
	rm(list = Objects, envir = .GlobalEnv)
}

"printObject" <- function ()
{
	Obj <- getVar(c("geomat", "geoshapes", "geopoints", "auremask",
		"aurelhy"), multi = FALSE,
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
	# Select an aurelhy object and display the PCA screeplot + summary
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
	# TODO: select nbr.pc and scale and update the aurelhy object
}

"updateLm" <- function ()
{
	# TODO: enter a model and update it in the object
}

"updateVgm" <- function ()
{
	# TODO: enter code for calculating a vgm() and update it in the object
}

"extractPca" <- function ()
{
	# TODO: extract one PCA component and format it as a geomat object
}

"extractInterpolated" <- function ()
{
	# TODO: extract interpolated data and convert to a geomat
}