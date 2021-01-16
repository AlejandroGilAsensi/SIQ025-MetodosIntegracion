#' Integral por la regla de Simpson.
#'
#' @param f Función a integrar.
#' @param a Origen del intervalo de integración.
#' @param b Final el intervalo de integración.
#' @param op Opción del método, o bien el op=1/3 o bien op=3/8.
#' @param n Número de subintervalos donde aplicar el método (por defecto n=1).
#'
#' @return Devuelve la integral de f entre a y b realizada por el método de Simpson 1/3 o 3/8, según el valor de op. Si n=1,
#'         se realiza el método simple, y si n > 1 se realiza el método compuesto.
#' @export
#'
#' @examples IntSimpson(cos,0,1), IntRect(cos,0,1,op=3/8,n=300)
#'
#' @seealso \link{IntRect}, \link{IntTrap}, \link{IntPM}
#'
IntSimpson = function(f,a,b,op=1/3,n=1){
  if (floor(n)!=n){
    stop("n debe ser 1 o un numero entero par")
  }
  if (op!=1/3 & op!=3/8){
    stop("op debe ser 1/3 o 3/8")
  }
  if (n==1){
  if (op==1/3){
    c=(b-a)/2
    return(c/3*(f(a)+4*f(c)+f(b)))
  }
  if (op==3/8){
    c=(b-a)/3
    return(3*c/8*(f(a)+3*f((2*a+b)/3)+3*f((a+2*b)/3)+f(b)))
  }
  }
  if (n!=1){
    if (op==1/3){
      if (floor(n/2)!=n/2){
        stop("n debe ser un numero par para aplicar la regla de Simpson 1/3 compuesta")
      }
      else{
        h = (b-a)/n
        int = f(a)+f(b)
        for (i in 1:(n-1)){
          if (floor(i/2)!=i/2){
            int = int + 4*f(a+i*h)
          }
          else{
            int = int + 2*f(a+i*h)
          }
        }
        return(int*h/3)
      }
      }
     if (op==3/8){
       if (floor(n/3)!=n/3){
         stop("n debe ser multiplo de 3 para aplicar la regla de Simpson 3/8 compuesta")
       }
       h = (b-a)/n
       int = f(a)+f(b)
       for (i in 1:(n-1)){
         if (floor(i/3)==i/3){
           int = int + 2*f(a+i*h)
         }
         else{
           int = int + 3*f(a+i*h)
         }
       }
       return(3/8*h*int)
     }
    }
  }


