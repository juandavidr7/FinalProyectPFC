import Oraculo._
import ReconstCadenas._
import ReconstCadenasPar._
import scala.util.{Try, Success, Failure}
import scala.util.Random
import Benchmark._

// =================================================================================
// ARCHIVO DE PRUEBAS, BENCHMARK Y COMPARATIVA DE SPEEDUP
// =================================================================================

// ---------------------------------------------------------------------------------
// 1. HERRAMIENTAS DE PRUEBA
// ---------------------------------------------------------------------------------

val random = new Random()

def secAlAzar(long: Int): Seq[Char] = {
  val longitudValida = math.max(0, long)
  (1 to longitudValida).map(_ => alfabeto(random.nextInt(alfabeto.length)))
}

// La función `medirTiempo` manual ha sido eliminada.
// En su lugar, usaremos `medirConResultado` del objeto Benchmark.

// ---------------------------------------------------------------------------------
// 2. FUNCIÓN PRINCIPAL DE COMPARACIÓN Y VERIFICACIÓN
// ---------------------------------------------------------------------------------

def compararYVerificar(
                        nombreAlgo: String,
                        secuencias: Seq[Seq[Char]],
                        fnSec: (Int, Oraculo) => Seq[Char],
                        fnPar: (Int, Oraculo) => Seq[Char]
                      ): Unit = {

  println(s"\n--- Comparativa: $nombreAlgo vs. ${nombreAlgo}Par ---")
  println(f"${"n"}%-8s ${"T. Sec (ms)"}%-15s ${"T. Par (ms)"}%-15s ${"Speedup"}%-12s ${"Costo"}%-8s ${"Resultado"}%-10s")
  println("-" * 80)

  for (s <- secuencias) {
    val n = s.length
    val costoOraculo = 0

    val o = crearOraculo(costoOraculo)(s)
    val resSec = Try(medirConResultado(fnSec(n, o)))
    val resPar = Try(medirConResultado(fnPar(n, o)))

    (resSec, resPar) match {
      case (Success((obtenidoSec, tSec)), Success((obtenidoPar, tPar))) =>
        val speedup = if (tPar > 0) tSec / tPar else 0.0
        val verifSec = obtenidoSec == s
        val verifPar = obtenidoPar == s

        val estado = (verifSec, verifPar) match {
          case (true, true)   => "ÉXITO"
          case (true, false)  => "FALLO (Par)"
          case (false, true)  => "FALLO (Sec)"
          case (false, false) => "FALLO (Ambos)"
        }

        print(f"$n%-8d ${tSec}%-15.4f ${tPar}%-15.4f ${speedup}%-11.2fx ${costoOraculo}%-8d ${estado}%-10s")
        if (!verifSec || !verifPar) {
          println()
          if (!verifSec) println(s"  - Sec: Esperado=${s.mkString}, Obtenido=${obtenidoSec.mkString}")
          if (!verifPar) println(s"  - Par: Esperado=${s.mkString}, Obtenido=${obtenidoPar.mkString}")
        } else {
          println()
        }

      case (Failure(e), _) =>
        println(f"$n%-8d ${"ERROR (Sec)"}%-15s ${"---"}%-15s ${"---"}%-12s ${costoOraculo}%-8d ${"FALLO"}%-10s")
        println(s"  - Excepción en Secuencial: ${e.getMessage}")
      case (_, Failure(e)) =>
        println(f"$n%-8d ${"---"}%-15s ${"ERROR (Par)"}%-15s ${"---"}%-12s ${costoOraculo}%-8d ${"FALLO"}%-10s")
        println(s"  - Excepción en Paralelo: ${e.getMessage}")
    }
  }
}

// =================================================================================
// 3. EJECUCIÓN DE LAS PRUEBAS Y BENCHMARKS
// =================================================================================

println("Generando secuencias de prueba...")

val secsCortas = (1 to 5).map(secAlAzar)
val secsParaMejorado = (2 to 5).map(i => secAlAzar(math.pow(2, i).toInt))
val secsLargas = (4 to 5).map(i => secAlAzar(math.pow(2, i).toInt))

println("Datos generados. Iniciando comparativas...")
println(s"Usando ${Runtime.getRuntime.availableProcessors()} procesadores para tareas paralelas.")
println("\nNota: El costo del oráculo se ajusta dinámicamente (0 para n grandes, 1 para n pequeños).")

// --- Ejecutar comparativas para cada par de algoritmos ---

compararYVerificar("Ingenuo", secsCortas, reconstruirCadenaIngenuo, reconstruirCadenaIngenuoPar)
compararYVerificar("Mejorado", secsParaMejorado, reconstruirCadenaMejorado, reconstruirCadenaMejoradoPar)
compararYVerificar("Turbo", secsLargas, reconstruirCadenaTurbo, reconstruirCadenaTurboPar)
compararYVerificar("TurboMejorada", secsLargas, reconstruirCadenaTurboMejorada, reconstruirCadenaTurboMejoradaPar)
compararYVerificar("TurboAcelerada", secsLargas, reconstruirCadenaTurboAcelerada, reconstruirCadenaTurboAceleradaPar)

println("\n--- Fin de las comparativas ---")
