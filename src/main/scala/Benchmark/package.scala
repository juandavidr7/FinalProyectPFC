import org.scalameter._
import Oraculo.Oraculo // Importamos el tipo Oraculo para usarlo en las firmas

package object Benchmark {

  /**
   * Compara una versión secuencial y una paralela de un algoritmo de reconstrucción.
   *
   * Esta función está diseñada específicamente para la firma de los algoritmos del proyecto.
   * Mide el tiempo de ejecución de ambas funciones con los mismos datos de entrada
   * y calcula el speedup.
   *
   * @param fnSecuencial La función secuencial, con firma (Int, Oraculo) => Seq[Char].
   * @param fnParalela   La función paralela, con firma currificada Int => (Int, Oraculo) => Seq[Char].
   * @param umbral       El umbral para decidir cuándo la función paralela debe usar paralelismo.
   * @param n            La longitud de la secuencia a reconstruir.
   * @param o            El oráculo ya creado para la secuencia.
   * @return Una tupla (tiempo secuencial, tiempo paralelo, speedup).
   */
  def compararAlgoritmos(
                          fnSecuencial: (Int, Oraculo) => Seq[Char],
                          fnParalela: Int => (Int, Oraculo) => Seq[Char]
                        )(umbral: Int, n: Int, o: Oraculo): (Double, Double, Double) = {

    // Configuración de ScalaMeter para mediciones directas sin calentamiento
    val cfg = config(
      KeyValue(Key.exec.minWarmupRuns -> 0),
      KeyValue(Key.exec.maxWarmupRuns -> 0),
      KeyValue(Key.verbose -> false)
    )

    // Medir el tiempo de la versión secuencial
    val tiempoSecuencial = cfg measure {
      fnSecuencial(n, o)
    }

    // Medir el tiempo de la versión paralela, aplicando sus parámetros
    val tiempoParalelo = cfg measure {
      fnParalela(umbral)(n, o)
    }

    // Calcular el speedup
    val speedup = tiempoSecuencial.value / tiempoParalelo.value

    (tiempoSecuencial.value, tiempoParalelo.value, speedup)
  }

  /**
   * Compara dos funciones genéricas de tipo (A => B), donde la entrada A
   * ya ha sido preparada externamente.
   * Útil para otros benchmarks (matrices, vectores, etc.).
   * Configura los warmup runs a 0.
   */
  def compararGenerico[A, B](
                              f1: A => B,
                              f2: A => B
                            )(input: A): (Double, Double, Double) = {
    val cfg = config(
      KeyValue(Key.exec.minWarmupRuns -> 0),
      KeyValue(Key.exec.maxWarmupRuns -> 0),
      KeyValue(Key.verbose -> false)
    )

    val t1 = cfg measure { f1(input) }
    val t2 = cfg measure { f2(input) }

    val speedup = t1.value / t2.value
    (t1.value, t2.value, speedup)
  }
}
