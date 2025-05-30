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
      case None => Seq.empty[Char]
    }
  }


  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {

    @annotation.tailrec
    def construirCandidatos(k: Int, scPrev: Seq[Seq[Char]]): Seq[Char] = {

      val scK: Seq[Seq[Char]] = for {
        prefijo <- scPrev // Toma cada subcadena válida de longitud k−1
        ch <- Oraculo.alfabeto // Prueba cada carácter posible del alfabeto
        candidato = prefijo :+ ch // Concatena el prefijo ++ ch para formar la nueva cadena
        if o(candidato) // Filtra sólo las que el oráculo (o) acepta
      } yield candidato // scK es la lista de todos los candidatas válidos

      // Busca si alguna candidato ya alcanzó longitud n
      scK.filter(_.length == n) match {
        case sol :: _ => sol
        case Nil => construirCandidatos(k + 1, scK)
      }
    }

    // Entrada: k = 1 (cadenas de longitud 1), scPrev = Seq(Seq.empty) (sólo la cadena vacía)
    construirCandidatos(1, Seq(Seq.empty[Char]))
  }
  
  //
//  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
//    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
//    // n debe ser potencia de 2
//    ???
//  }
//
//  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
//    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
//    // Usa el filtro para ir más rápido
//    // n debe ser potencia de 2
//    ???
//  }
//
//  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
//    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
//    // Usa el filtro para ir más rápido
//    // Usa árboles de sufijos para guardar Seq[Seq[Char]]
//    // n debe ser potencia de 2
//    ???
//  }

}
