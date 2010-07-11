\name{gui}
\alias{AMDlg}
\alias{aboutAgromet}
\alias{exitAgromet}
\alias{closeAgrometAssistant}
\alias{closeAgromet}
\alias{focusR}
\alias{focusGraph}
\alias{loadObjects}
\alias{saveObjects}
\alias{listObjects}
\alias{removeObjects}
\alias{printObject}
\alias{getVar}
\alias{viewAuremask}
\alias{viewAurelhy}
\alias{viewAurelhyPredict}
\alias{updatePca}
\alias{updateLm}
\alias{updateVgm}
\alias{extractPca}
\alias{extractInterpolated}

\title{ The AgroMet GUI (Graphical User Interface) }
\description{
  These function display menus and dialog boxes to access AgroMet function
  without programming. Most of them are not intended to be called directly.
}
\usage{
AMDlg()
aboutAgromet(graphical = FALSE)

exitAgromet()
closeAgromet()
closeAgrometAssistant()

focusR()
focusGraph()

loadObjects()
saveObjects()
listObjects()
removeObjects()
printObject()

getVar(class = "data.frame", default = "", multi = FALSE,
    title = paste("Choose a ", class, ":", sep = ""), warn.only = TRUE)

viewAuremask()
viewAurelhy()
viewAurelhyPredict()

updatePca()
updateLm()
updateVgm()

extractPca()
extractInterpolated()
}

\arguments{
  \item{graphical}{ If \code{TRUE}, a graphical dialog box displays the
    information. Otherwise, information is printed at the R console. }
  \item{class}{ The class of objects to retrieve }
  \item{default}{ The default item selected in the list }
  \item{multi}{ Are multiple selections allowed? }
  \item{title}{ The title of the dialog box }
  \item{warn.only}{ Issue a warning if there is a problem, otherwise, the
    function stops execution. This is used in the AgroMet GUI, in order to
	avoid interrupting abruptly code execution and to report the error in a more
	"civilized" way to the user }
}

\value{
  \code{AMDlg()} is called for its side effect of displaying the main AgroMet
    window.
  \code{AboutAgromet()} gives some information about the current AgroMet version.
  \code{ExitAgromet()} unloads the agromet package.
}

\author{ Philippe Grosjean <Philippe.Grosjean@umons.ac.be> }

\seealso{ \code{\link{agromet}} }
\examples{
# The dialog box can be started just by issuing
# > AMDlg()
}
\keyword{ utilities }
