import org.scalameter._
import Oraculo._


// Se define como un objeto para que sea fácilmente importable
package object Benchmark {

  def medirConResultado[A](block: => A): (A, Double) = {
    val cfg = config(
      Key.exec.minWarmupRuns := 0, // CORREGIDO
      Key.exec.maxWarmupRuns := 0, // CORREGIDO
      Key.verbose := false         // CORREGIDO
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
      Key.exec.minWarmupRuns := 0, // CORREGIDO
      Key.exec.maxWarmupRuns := 0, // CORREGIDO
      Key.verbose := false         // CORREGIDO
    )

    val tiempoSecuencial = cfg measure {
      fnSecuencial(n, o)
    }

    val tiempoParalelo = cfg measure {
      fnParalela(umbral)(n, o)
    }

    val speedup = tiempoSecuencial.value / tiempoParalelo.value

    (tiempoSecuencial.value, tiempoParalelo.value, speedup)
  }

  def compararGenerico[A, B](
                              f1: A => B,
                              f2: A => B
                            )(input: A): (Double, Double, Double) = {
    val cfg = config(
      Key.exec.minWarmupRuns := 0, // CORREGIDO
      Key.exec.maxWarmupRuns := 0, // CORREGIDO
      Key.verbose := false         // CORREGIDO
    )

    val t1 = cfg measure { f1(input) }
    val t2 = cfg measure { f2(input) }

    val speedup = t1.value / t2.value
    (t1.value, t2.value, speedup)
  }
}
