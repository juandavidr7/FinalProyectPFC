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
    candidatos.find(o) match {
      case Some(cadena) => cadena
      case None => throw new NoSuchElementException("¡El oráculo no reconoció ninguna cadena!")
    }
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {

    // Generar todas las secuencias válidas desde longitud 1 hasta n
    val todas: Seq[Seq[Char]] = (1 to n).foldLeft(Seq(Seq.empty[Char])) {

      (candidatosPrevios, _) =>
        for {
          prefijo <- candidatosPrevios
          c <- alfabeto
          nuevo = prefijo :+ c
          if o(nuevo)
        } yield nuevo
    }


    // Buscar una secuencia válida de longitud n
    todas.find(o) match {
      case Some(cadena) => cadena
      case None => throw new NoSuchElementException("¡No se encontró ninguna cadena aceptada por el oráculo!")
    }
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {

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
