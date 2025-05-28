import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // Genera todas las cadenas de longitud exactamente n sobre el alfabeto
    val candidatos: Seq[Seq[Char]] =
      (1 to n).foldLeft(Seq(Seq.empty[Char])) { (acc, _) =>
        for {
          prefijo <- acc
          c <- alfabeto
        } yield prefijo :+ c
      }

    // Busca la primera cadena aceptada por el oráculo
    candidatos //Esto lanza error porque la función debe devolver Seq[Char] xd, entonces tenemos que hacerle
    // un proceso con el .find(o) y un patrón match
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    ???
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    //Comprobación de que la longitud es una potencia de 2
    require((n & (n - 1)) == 0, "La longitud debe ser una potencia de 2")
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // n debe ser potencia de 2
    val alfabeto = Oraculo.alfabeto

    def generarConjuntos(k:Int):Seq[Seq[Char]] = {
      if (k == 1) alfabeto.map(Seq(_))
      else {
        val subcadena = generarConjuntos(k/2)
        for {
          a <- subcadena
          b <- subcadena
        } yield a++b
      }
    }
    def turbo(k:Int, actual: Seq[char]): Seq[char] ={
      if (k==0) actual
      else{
        val candidatas = generarConjuntos(k)
        val cadenasValidas = candidatas.find(b=> o(actual++b))
        cadenasValidas match{
          case Some(cadena) => turbo(k/2, actual++cadena)
          case none =>{
            //Si no hay cadenas válidas ya se terminó
            actual
          }
        }
      }
      turbo(n/2, Seq.empty[Char])
    } 
  }

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa el filtro para ir más rápido
    // n debe ser potencia de 2
    ???
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa el filtro para ir más rápido
    // Usa árboles de sufijos para guardar Seq[Seq[Char]]
    // n debe ser potencia de 2
    ???
  }

}
