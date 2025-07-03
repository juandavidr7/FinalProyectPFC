// Guardar como Pruebas.scala
import Oraculo._
import ReconstCadenas._
import ReconstCadenasPar._
import scala.util.{Try, Success, Failure}
import scala.util.Random

// =================================================================================
// ARCHIVO DE PRUEBAS, BENCHMARK Y COMPARATIVA DE SPEEDUP (CON COSTE DE ORÁCULO DINÁMICO)
// =================================================================================

// ---------------------------------------------------------------------------------
// 1. HERRAMIENTAS DE PRUEBA
// ---------------------------------------------------------------------------------

val random = new Random()

def secAlAzar(long: Int): Seq[Char] = {
  val longitudValida = math.max(0, long)
  (1 to longitudValida).map(_ => alfabeto(random.nextInt(alfabeto.length)))
}

def medirTiempo[A](block: => A): (A, Double) = {
  val t0 = System.nanoTime()
  val resultado = block
  val t1 = System.nanoTime()
  val tiempoMs = (t1 - t0) / 1e6
  (resultado, tiempoMs)
}

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

    // --- LÓGICA DE COSTE DINÁMICO ---
    // Determinar el costo del oráculo para evitar tiempos de ejecución excesivos en pruebas grandes,
    // pero manteniendo un coste realista para las pequeñas.
    val costoOraculo = nombreAlgo match {
      case "Ingenuo" if n > 12 => 0
      case "Mejorado" if n > 18 => 0
      case "Turbo" | "TurboMejorada" | "TurboAcelerada" if n > 1024 => 0
      case _ => 1 // Costo por defecto para el resto de casos
    }

    val o = crearOraculo(costoOraculo)(s)

    val resSec = Try(medirTiempo(fnSec(n, o)))
    val resPar = Try(medirTiempo(fnPar(n, o)))

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

val secsCortas = (1 to 13).map(secAlAzar) // Hasta n=15 para Ingenuo
val secsParaMejorado = (10 to 20 by 2).map(secAlAzar)
val secsLargas = (4 to 11).map(i => secAlAzar(math.pow(2, i).toInt)) // 16, 32, ..., 4096

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