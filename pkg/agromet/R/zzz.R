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

".onAttach" <- function (libname, pkgname)
{
	if (!interactive()) options(AMAssistant  = FALSE)

	# Use the SciViews style for dialog boxes
	options(guiStyle = "SciViews")

	# Create some strings in TempEnv
	AMversion <- "1.0-0"
	assignTemp("AMversion", AMversion)

	AMname <- "AgroMet"
	assignTemp("AMname", AMname)
	assignTemp("AMverstring", paste(AMname, "version", AMversion))

	AMetc <- file.path(.path.package(package = "agromet")[1], "etc")
	assignTemp("AMetc", AMetc)

	AMgui <- file.path(.path.package(package = "agromet")[1], "gui")
	assignTemp("AMgui", AMgui)

	# Windows specific things
	if (isWin()) {
		if (interactive()) {
			AMico <- tk2ico.create(file.path(getTemp("AMgui"),
				"AgroMet.ico"))
			assignTemp("AMico", AMico)
		}

		# Make sure there is a key for AgroMet in the registry
		AMkey <- "HKEY_LOCAL_MACHINE\\Software\\AgroMet"
		tk2reg.setkey(AMkey)
		assignTemp("AMkey", AMkey)
	}

	# Load the various image resources
	if (interactive()) ImgReadPackage("agromet")

	# Load the menus
	if (interactive()) MenuReadPackage("agromet")

	# Possibly create the AMguiPackage variable to indicate from where to load
	# other GUI resources
	AMguiPackage <- "agromet"
	assignTemp("AMguiPackage", AMguiPackage)

	# Possibly load the AgroMet assistant
	LoadIt <- getOption("AMAssistant")
	if (is.null(LoadIt) || LoadIt == TRUE) AMDlg()
}

# Unloading ZooImage
".onUnload" <- function (libpath)
{
	# Eliminate the AgroMet menu entries
	if (.Platform$GUI[1] == "Rgui") {
		try(MenuDel("$ConsoleMain/AgroMet"), silent = TRUE)
		try(MenuDel("$ConsolePopup/AgroMet"), silent = TRUE)
	}
	# Destroy the AgroMet Tk window, if it is currently displayed
	tkWinDel("AMDlgWin")
}
