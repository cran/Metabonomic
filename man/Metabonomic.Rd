\name{Metabonomic}
\alias{Metabonomic GUI}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ GUI for the Metabonomic Analysis}
\description{
Graphical User Interface for the Metabonomic Analysis (Baseline, Normalization, Peak Detection,
  PCA, PLS, Nearest Neigbourgt, Neural Network) developed to make easy this data analysis.
}
\usage{
Metabonomic()
}

\details{
  
The ''Metabonomic'' GUI was designed using the R-Tcl/Tk interface that allows using the TK toolkit, replacing Tcl code with R function calls. The purpose was to allow an easy interaction with the R functions as well as a comprehensive metabonomic analysis. The software offers several graphic outputs and the different plots are created using a combination of different Tcl/Tk interfaces.  The program has been created with R version 2.8.0 under Windows operating system.
To use the ''Metabonomic'' GUI, the ''Metabonomic'' package and all the required packages have to be downloaded and installed in the R console. The required PROcess package can be found in the Bioconductor Project Site . Next, the ''Metabonomic'' package has to be loaded using the Package installer or writing ''>require (Metabonomic)'' if the package is already in the computer.  
The program is started by writing ''> Metabonomic()'' in the R console, which generates the main user interface . The main GUI has an input console to launch any R applications and two different output consoles for the warnings and for the output messages. The GUI also has a button line, where the following buttons are available: (a) undo, (b) redo, (c) current data display, (d) launch the commands written in the input console, (e) erase the input console, (f) stop any running process and (g) shutdown  the GUI and return to R console.
Finally, the main GUI has a main menu with different tabs: File, Script, Edit, Preprocessing, Metabonomic Analysis and Spectrum. From the Script tab, the following functions are available: (a) ''Load a Script'' opens a script into the input console, (b) ''Save Script'' saves the commands written in the input console as an R script file and (c) ''Launch the Script'' runs the commands written in the input console.
}

\author{  Jose L. Izquierdo \email{izquierdo@ieb.ucm.es} }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

\keyword{ interface }
