% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parser.R
\docType{class}
\name{ASTFunction}
\alias{ASTFunction}
\title{A specified function call as an ASTNode}
\format{
\code{\link{R6Class}} object.
}
\description{
A specified function call as an ASTNode

A specified function call as an ASTNode
}
\examples{
ASTFunction$new("log", "x+2")$string()

}
\keyword{data}
\section{Super class}{
\code{\link[tangram:ASTNode]{tangram::ASTNode}} -> \code{ASTFunction}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{r_expr}}{A string containing the raw r expression from inside the parenthesis}

\item{\code{data}}{Data stored as a result of reduction}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{ASTFunction$new()}}
\item \href{#method-factors}{\code{ASTFunction$factors()}}
\item \href{#method-name}{\code{ASTFunction$name()}}
\item \href{#method-string}{\code{ASTFunction$string()}}
\item \href{#method-reduce}{\code{ASTFunction$reduce()}}
\item \href{#method-clone}{\code{ASTFunction$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
\item \out{<span class="pkg-link" data-pkg="tangram" data-topic="ASTNode" data-id="distribute">}\href{../../tangram/html/ASTNode.html#method-distribute}{\code{tangram::ASTNode$distribute()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tangram" data-topic="ASTNode" data-id="set_format">}\href{../../tangram/html/ASTNode.html#method-set_format}{\code{tangram::ASTNode$set_format()}}\out{</span>}
\item \out{<span class="pkg-link" data-pkg="tangram" data-topic="ASTNode" data-id="terms">}\href{../../tangram/html/ASTNode.html#method-terms}{\code{tangram::ASTNode$terms()}}\out{</span>}
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Construct a node representing a function call
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ASTFunction$new(value, r_expr)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{value}}{(character) The name of the function call}

\item{\code{r_expr}}{Any r expression to be evaluated inside the call}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-factors"></a>}}
\if{latex}{\out{\hypertarget{method-factors}{}}}
\subsection{Method \code{factors()}}{
Returns all terminal nodes, this is a terminal node so returns self
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ASTFunction$factors()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-name"></a>}}
\if{latex}{\out{\hypertarget{method-name}{}}}
\subsection{Method \code{name()}}{
Returns the function call as character
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ASTFunction$name()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-string"></a>}}
\if{latex}{\out{\hypertarget{method-string}{}}}
\subsection{Method \code{string()}}{
Returns a re-parsable representation of the node
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ASTFunction$string()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-reduce"></a>}}
\if{latex}{\out{\hypertarget{method-reduce}{}}}
\subsection{Method \code{reduce()}}{
Given a data.frame execute the function in that environment and associate the result as data.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ASTFunction$reduce(data)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data}}{(data.frame) The data.frame to use as the enviroment for the function execution}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{ASTFunction$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
