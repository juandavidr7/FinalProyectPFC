import org.scalameter.*
import org.scalameter.KeyValue._
import Oraculo._

// Se define como un objeto para que sea fácilmente importable
package object Benchmark {
  
  def medirConResultado[A](block: => A): (A, Double) = {
    val cfg = config(
      Key.exec.minWarmupRuns -> 0,
      Key.exec.maxWarmupRuns -> 0,
      Key.verbose -> false
    )
    var resultado: A = null.asInstanceOf[A]
    val tiempo = cfg measure {
      resultado = block
    }
    (resultado, tiempo.value)
  }
  

  def compararAlgoritmos(
                          fnSecuencial: (Int, Oraculo) => Seq[Char],
                          fnParalela: Int => (Int, Oraculo) => Seq[Char]
                        )(umbral: Int, n: Int, o: Oraculo): (Double, Double, Double) = {

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