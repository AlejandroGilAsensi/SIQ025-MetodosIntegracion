% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IntSimpson.R
\name{IntSimpson}
\alias{IntSimpson}
\title{Integral por la regla de Simpson.}
\usage{
IntSimpson(f, a, b, op = 1/3, n = 1)
}
\arguments{
\item{f}{Función a integrar.}

\item{a}{Origen del intervalo de integración.}

\item{b}{Final el intervalo de integración.}

\item{op}{Opción del método, o bien el op=1/3 o bien op=3/8.}

\item{n}{Número de subintervalos donde aplicar el método (por defecto n=1).}
}
\value{
Devuelve la integral de f entre a y b realizada por el método de Simpson 1/3 o 3/8, según el valor de op. Si n=1,
se realiza el método simple, y si n > 1 se realiza el método compuesto.
}
\description{
Integral por la regla de Simpson.
}
\examples{
IntSimpson(cos,0,1), IntRect(cos,0,1,op=3/8,n=300)

}
\seealso{
\link{IntRect}, \link{IntTrap}, \link{IntPM}
}
