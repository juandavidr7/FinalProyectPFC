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
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa paralelismo de tareas y/o datos
    ???
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
