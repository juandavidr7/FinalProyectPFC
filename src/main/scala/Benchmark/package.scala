import org.scalameter._
import Oraculo._

package object Benchmark{

// Benchmark
def medirTiempo[A](bloque: => A): Double = {
  config(
    KeyValue(Key.exec.minWarmupRuns -> 5), // Reducido para pruebas más rápidas
    KeyValue(Key.exec.maxWarmupRuns -> 15),
    KeyValue(Key.verbose -> false)
  ).withWarmer(new Warmer.Default).measure(bloque).value
}
}