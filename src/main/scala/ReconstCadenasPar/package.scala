import common._
import scala.collection.parallel.CollectionConverters._
import Oraculo._
import ArbolSufijos._

package object ReconstCadenasPar {

  // Versión paralela ingenua
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Usa paralelismo de tareas
    ???
  }

  // Versión paralela mejorada
  def reconstruirCadenaMejoradoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {

    @annotation.tailrec
    def construirCandidatos(k: Int, scPrev: Seq[Seq[Char]]): Seq[Char] = {
      // Decidir si paralelizar o no
      val fuente = {
        if (scPrev.size > umbral) scPrev.par
        else scPrev
      }

      // Generación y filtrado con expresión for
      // Si "fuente" es ParSeq, correrá en paralelo.
      val scK: Seq[Seq[Char]] = (for {
        prefijo <- fuente
        ch <- Oraculo.alfabeto
        candidato = prefijo :+ ch
        if o(candidato)
      } yield candidato).toList

      // Buscar solución de longitud n usando match
      scK.filter(_.length == n) match {
        case sol :: _ => sol
        case Nil => construirCandidatos(k + 1, scK)
      }
    }

    // Punto de entrada
    construirCandidatos(1, Seq(Seq.empty[Char]))
  }

  // Versión paralela turbo
  def reconstruirCadenaTurboPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa paralelismo de tareas y/o datos
    // n debe ser potencia de 2
    ???
  }

  // Versión paralela turbo mejorada
  def reconstruirCadenaTurboMejoradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa paralelismo de tareas y/o datos
    // n debe ser potencia de 2
    ???
  }

  // Versión paralela turbo acelerada
  def reconstruirCadenaTurboAceleradaPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa árboles de sufijos para guardar Seq[Seq[Char]]
    // Usa paralelismo de tareas y/o datos
    // n debe ser potencia de 2
    ???
  }

}
