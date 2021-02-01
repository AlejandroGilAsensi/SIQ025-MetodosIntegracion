#' Integral por la regla del punto medio.
#'
#' @param f Función a integrar.
#' @param a Origen del intervalo de integración.
#' @param b Final el intervalo de integración.
#' @param n Número de subintervalos donde aplicar el método (por defecto n=1).
#'
#' @return Devuelve la integral de f entre a y b realizada por la regla del punto medio sobre n subintervalos equidistantes entre a y b.
#' @export
#'
#' @examples IntPM(cos,0,1) IntPM(cos,0,1,n=4)
#'
#' @seealso \link{IntRect}, \link{IntTrap}, \link{IntSimpson}
#'
IntPM = function(f,a,b,n=1){
  if (floor(n)!=n){
    stop("n debe ser un numero entero positivo")
  }
  h=(b-a)/n
  int=0
  xi = a
  xj = a + h
  for (i in 1:n){
    c = (xi + xj)/2
    int = int + f(c)*h
    xi = xi + h
    xj = xj + h
  }
  return(int)
}

