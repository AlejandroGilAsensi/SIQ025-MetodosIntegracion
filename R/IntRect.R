#' Integral por la regla del rectángulo.
#'
#' @param f Función a integrar.
#' @param a Origen del intervalo de integración.
#' @param b Final el intervalo de integración.
#' @param n Número de subintervalos donde aplicar el método (por defecto n=1).
#'
#' @return Devuelve la integral de f entre a y b realizada por la regla del rectángulo sobre n subintervalos equidistantes entre a y b.
#' @export
#'
#' @examples IntRect(cos,0,1) IntRect(cos,0,1,n=4)
#'
#' @seealso \link{IntPM}, \link{IntTrap}, \link{IntSimpson}
IntRect <- function(f,a,b,n=1){
  if (floor(n)!=n){
    stop("n debe ser un numero entero positivo")
  }
  h=(b-a)/n
  int=0
  for (i in 1:n){
    int = int + f(a+(i-1)*h)*h
  }
  return(int)
}


