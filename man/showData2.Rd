\name{showData2}
\alias{showData2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Display a Data Frame in a Tk Text Widget}
\description{
  Modification of the showData function of the relimp package. 
}
\usage{
showData2(dataframe, colname.bgcolor , rowname.bgcolor , body.bgcolor , colname.textcolor , rowname.textcolor , body.textcolor , font , maxheight, maxwidth , title , rowname.bar , colname.bar, rownumbers , placement, suppress.X11.warnings)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataframe}{ A data frame, or an object to which as.data.frame() can be validly applied}
  \item{colname.bgcolor}{ A background colour for the variable\_names panel}
  \item{rowname.bgcolor}{ A background colour for the row\_names panel}
  \item{body.bgcolor}{ A background colour for the data}
  \item{colname.textcolor}{ A colour for the variable names}
  \item{rowname.textcolor}{ A colour for the row names}
  \item{body.textcolor}{ A colour for the data}
  \item{font}{ The text font used . should be a monospaced font}
  \item{maxheight}{The maximum number of rows to display}
  \item{maxwidth}{ The maximum width of display, in characters}
  \item{title}{ A title for the window. Default is to use the name of the dataframe as given in the call to showData()}
  \item{rowname.bar}{ position of sidebar for row names, ''left'' or ''right'', or c(''left'',''right''), or NULL}
  \item{colname.bar}{position of column names, ''top'' or ''bottom'', or c(''top'',''bottom''), or NULL}
  \item{rownumbers}{ logical, whether row numbers should be displayed}
  \item{placement}{ Position of the bottom right corner of the window}
  \item{suppress.X11.warnings}{ logical, if TRUE then any X11 warnings are suppressed}
}

\value{
  \item{invisible }{NULL}
}
\references{ 
relimp package  \url{http://finzi.psych.upenn.edu/R/library/relimp/html/showData.html}
}
\keyword{ interface }

