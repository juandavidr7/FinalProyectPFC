import common.*
import scala.collection.parallel.CollectionConverters.*
import Oraculo.*
import ArbolSufijos.*
import scala.annotation.tailrec


package object ReconstCadenasPar {

  // Versión paralela ingenua
  def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, o: Oraculo): Seq[Char] = {
    val candidatos: Seq[Seq[Char]] =
      (1 to n).foldLeft(Seq(Seq.empty[Char])) { (acc, _) =>
        for {
          prefijo <- acc
          c <- alfabeto
        } yield prefijo :+ c
      }

    val buscar = if (candidatos.size >= umbral) candidatos.par else candidatos
    buscar.iterator.find(o) match {
      case Some(cadena) => cadena
      case None => Seq.empty[Char]
    }
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
    // Validar que n debe ser potencia de 2 y positiva
    require(n > 0 && (n & (n - 1)) == 0, "La longitud n debe ser una potencia de 2 y mayor que 0")

    // Construir SC1 = Σ filtrado por el oráculo
    val subcadenasValidasLongitudUno: Set[Seq[Char]] =
      Oraculo.alfabeto.map(ch => Seq(ch)).filter(s => o(s)).toSet

    // Caso base n == 1
    if (n == 1) {
      if (subcadenasValidasLongitudUno.isEmpty) {
        throw new RuntimeException(
          "No se encontró ninguna subcadena válida de longitud 1 a partir del alfabeto. No se puede reconstruir."
        )
      }
      // Se devuelve la única letra válida
      return subcadenasValidasLongitudUno.head
    }

    @tailrec
    def internal_turbo(subcadenasValidasAnteriores: Set[Seq[Char]], k: Int): Seq[Char] = {
      // Si k > n, revisa subcadenasValidasAnteriores para encontrar la cadena de longitud n
      if (k > n) {
        val solucionOpt: Option[Seq[Char]] =
          if (subcadenasValidasAnteriores.size > umbral) {
            // Busca en paralelo porque hay suficientes elementos
            subcadenasValidasAnteriores.par.find(s => s.length == n && o(s))
          } else {
            // Busca de forma secuencial
            subcadenasValidasAnteriores.find(s => s.length == n && o(s))
          }

        solucionOpt match {
          case Some(sol) => return sol
          case None => throw new RuntimeException("No se encontró solución de longitud n")
        }
      }

      // Generar SCk = SCk/2 × SCk/2, filtrando con el oráculo.
      // Se paraleliza la concatenación y el filtro si subcadenasValidasAnteriores.size > umbral
      val concatenaciones: Iterable[Seq[Char]] =
        if (subcadenasValidasAnteriores.size > umbral) {
          // En paralelo, pero luego se convierte a Seq normal con .seq
          (for {
            s1 <- subcadenasValidasAnteriores.par
            s2 <- subcadenasValidasAnteriores.par
            cadena = s1 ++ s2
            if o(cadena)
          } yield cadena).seq
        } else {
          // Secuencial
          for {
            s1 <- subcadenasValidasAnteriores
            s2 <- subcadenasValidasAnteriores
            cadena = s1 ++ s2
            if o(cadena)
          } yield cadena
        }

      // Convertir las concatenaciones (sea ParSet o Set) a un Set normal
      val candidatos: Set[Seq[Char]] = concatenaciones.toSet

      // Buscar en 'candidatos' alguna cadena de longitud n
      val solucionOpt2: Option[Seq[Char]] =
        if (candidatos.size > umbral) {
          // Búsqueda en paralelo
          candidatos.par.find(w => w.length == n && o(w))
        } else {
          // Búsqueda secuencial
          candidatos.find(w => w.length == n && o(w))
        }

      if (solucionOpt2.isDefined) {
        return solucionOpt2.get
      }

      // Filtrar 'candidatos' para formar SCk (solo aquellas con longitud exacta = k y válidas por oráculo)
      val nuevasSubcadenasSeq: Iterable[Seq[Char]] =
        if (candidatos.size > umbral) {
          // Filtrado paralelo, luego se convierte a secuencial con .seq
          candidatos.par.filter(w => w.length == k && o(w)).seq
        } else {
          // Filtrado secuencial
          candidatos.filter(w => w.length == k && o(w))
        }

      val conjuntoNuevasSubcadenas: Set[Seq[Char]] = nuevasSubcadenasSeq.toSet

      if (conjuntoNuevasSubcadenas.isEmpty) {
        throw new RuntimeException(s"No se encontraron subcadenas válidas de longitud k=$k")
      }

      // Recursión de cola: duplica k y usa SCk como SCk/2 en la siguiente iteración
      internal_turbo(conjuntoNuevasSubcadenas, k * 2)
    }

    // Llamada inicial: SC1 = subcadenasValidasLongitudUno, k = 2
    internal_turbo(subcadenasValidasLongitudUno, 2)
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
