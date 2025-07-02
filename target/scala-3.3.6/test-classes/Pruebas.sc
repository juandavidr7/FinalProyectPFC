import Oraculo._
import ReconstCadenas._
import scala.util.Random

// =================================================================================
// ARCHIVO DE PRUEBAS FUNCIONALES PARA ReconstCadenas
//
// Estructura:
// 1. Herramientas de Prueba: Funciones para generar datos y verificar resultados.
// 2. Generación de Datos: Creación de secuencias de prueba con nombres claros.
// 3. Suites de Pruebas: Bloques de pruebas dedicados a cada función,
//    mostrando su rendimiento de forma progresiva.
// =================================================================================


// ---------------------------------------------------------------------------------
// 1. HERRAMIENTAS DE PRUEBA
// ---------------------------------------------------------------------------------

val random = new Random()
val costoOraculo = 0 // No nos interesa medir el costo del oráculo en estas pruebas

/**
 * Genera una secuencia aleatoria de una longitud dada.
 * @param long Longitud de la secuencia a generar.
 * @return Una secuencia de caracteres aleatoria.
 */
def secAlAzar(long: Int): Seq[Char] = {
  (1 to long).map(_ => alfabeto(random.nextInt(alfabeto.length)))
}

/**
 * Función de ayuda para verificar y mostrar el resultado de una prueba de forma clara.
 * @param nombrePrueba Descripción de la prueba.
 * @param esperado La secuencia secreta que se debería encontrar.
 * @param obtenido El resultado de la función de reconstrucción.
 */
def verificar(nombrePrueba: String, esperado: Seq[Char], obtenido: Seq[Char]): Unit = {
  print(f"$nombrePrueba%-50s") // Imprime el nombre de la prueba alineado
  if (esperado == obtenido) {
    println(s"ÉXITO. (Longitud: ${obtenido.length})")
  } else {
    println(s"FALLO.")
    println(s"  - Esperado: ${esperado.mkString}")
    println(s"  - Obtenido: ${obtenido.mkString}")
  }
}

// ---------------------------------------------------------------------------------
// 2. GENERACIÓN DE DATOS DE PRUEBA
// ---------------------------------------------------------------------------------
println("Generando secuencias de prueba...")

// --- Secuencias para pruebas de funcionalidad y rendimiento ---
val sec_n4 = secAlAzar(4)
val sec_n8 = secAlAzar(8)
val sec_n16 = secAlAzar(16)
val sec_n32 = secAlAzar(32)
val sec_n256 = secAlAzar(256)
val sec_n512 = secAlAzar(512)
val sec_n1024 = secAlAzar(1024)
val sec_n2048 = secAlAzar(2048)
val sec_n4096 = secAlAzar(4096)

println("Datos generados. Iniciando pruebas...")


// =================================================================================
// 3. SUITES DE PRUEBAS PROGRESIVAS
// =================================================================================

// ---------------------------------------------------------------------------------
// Pruebas para: reconstruirCadenaIngenuo
// Objetivo: Demostrar que funciona para n muy pequeños.
// ---------------------------------------------------------------------------------
println("\n--- Pruebas para reconstruirCadenaIngenuo ---")
val oraculo_ingenuo_n4 = crearOraculo(costoOraculo)(sec_n4)
verificar("Prueba (Ingenuo) con n=4", sec_n4, reconstruirCadenaIngenuo(sec_n4.length, oraculo_ingenuo_n4))

val oraculo_ingenuo_n8 = crearOraculo(costoOraculo)(sec_n8)
verificar("Prueba (Ingenuo) con n=8", sec_n8, reconstruirCadenaIngenuo(sec_n8.length, oraculo_ingenuo_n8))
println("-> 'Ingenuo' es inviable para n > 10.")

// ---------------------------------------------------------------------------------
// Pruebas para: reconstruirCadenaMejorado
// Objetivo: Demostrar que es una mejora, pero aún no es viable para n grandes.
// ---------------------------------------------------------------------------------
println("\n--- Pruebas para reconstruirCadenaMejorado ---")
val oraculo_mejorado_n16 = crearOraculo(costoOraculo)(sec_n16)
verificar("Prueba (Mejorado) con n=16", sec_n16, reconstruirCadenaMejorado(sec_n16.length, oraculo_mejorado_n16))

val oraculo_mejorado_n32 = crearOraculo(costoOraculo)(sec_n32)
verificar("Prueba (Mejorado) con n=32", sec_n32, reconstruirCadenaMejorado(sec_n32.length, oraculo_mejorado_n32))
println("-> 'Mejorado' es inviable para n > 32-64 debido a su complejidad espacial.")

// ---------------------------------------------------------------------------------
// Pruebas para: reconstruirCadenaTurbo
// Objetivo: Demostrar su viabilidad en tamaños medianos.
// ---------------------------------------------------------------------------------
println("\n--- Pruebas para reconstruirCadenaTurbo ---")
val oraculo_turbo_n256 = crearOraculo(costoOraculo)(sec_n256)
verificar("Prueba (Turbo) con n=256", sec_n256, reconstruirCadenaTurbo(sec_n256.length, oraculo_turbo_n256))

val oraculo_turbo_n512 = crearOraculo(costoOraculo)(sec_n512)
verificar("Prueba (Turbo) con n=512", sec_n512, reconstruirCadenaTurbo(sec_n512.length, oraculo_turbo_n512))

val oraculo_turbo_n1024 = crearOraculo(costoOraculo)(sec_n1024)
verificar("Prueba (Turbo) con n=1024", sec_n1024, reconstruirCadenaTurbo(sec_n1024.length, oraculo_turbo_n1024))

val oraculo_turbo_n2048 = crearOraculo(costoOraculo)(sec_n2048)
verificar("Prueba (Turbo) con n=2048", sec_n2048, reconstruirCadenaTurbo(sec_n2048.length, oraculo_turbo_n2048))

// ---------------------------------------------------------------------------------
// Pruebas para: reconstruirCadenaTurboMejorada
// Objetivo: Demostrar su eficiencia superior en tamaños grandes.
// ---------------------------------------------------------------------------------
println("\n--- Pruebas para reconstruirCadenaTurboMejorada ---")
val oraculo_turboM_n256 = crearOraculo(costoOraculo)(sec_n256)
verificar("Prueba (TurboMejorada) con n=256", sec_n256, reconstruirCadenaTurboMejorada(sec_n256.length, oraculo_turboM_n256))

val oraculo_turboM_n512 = crearOraculo(costoOraculo)(sec_n512)
verificar("Prueba (TurboMejorada) con n=512", sec_n512, reconstruirCadenaTurboMejorada(sec_n512.length, oraculo_turboM_n512))

val oraculo_turboM_n1024 = crearOraculo(costoOraculo)(sec_n1024)
verificar("Prueba (TurboMejorada) con n=1024", sec_n1024, reconstruirCadenaTurboMejorada(sec_n1024.length, oraculo_turboM_n1024))

val oraculo_turboM_n2048 = crearOraculo(costoOraculo)(sec_n2048)
verificar("Prueba (TurboMejorada) con n=2048", sec_n2048, reconstruirCadenaTurboMejorada(sec_n2048.length, oraculo_turboM_n2048))

// ---------------------------------------------------------------------------------
// Pruebas para: reconstruirCadenaTurboAcelerada
// Objetivo: Demostrar que es la versión más performante y escala a tamaños muy grandes.
// ---------------------------------------------------------------------------------
println("\n--- Pruebas para reconstruirCadenaTurboAcelerada ---")
val oraculo_turboA_n256 = crearOraculo(costoOraculo)(sec_n256)
verificar("Prueba (TurboAcelerada) con n=256", sec_n256, reconstruirCadenaTurboAcelerada(sec_n256.length, oraculo_turboA_n256))

val oraculo_turboA_n512 = crearOraculo(costoOraculo)(sec_n512)
verificar("Prueba (TurboAcelerada) con n=512", sec_n512, reconstruirCadenaTurboAcelerada(sec_n512.length, oraculo_turboA_n512))

val oraculo_turboA_n1024 = crearOraculo(costoOraculo)(sec_n1024)
verificar("Prueba (TurboAcelerada) con n=1024", sec_n1024, reconstruirCadenaTurboAcelerada(sec_n1024.length, oraculo_turboA_n1024))

val oraculo_turboA_n2048 = crearOraculo(costoOraculo)(sec_n2048)
verificar("Prueba (TurboAcelerada) con n=2048", sec_n2048, reconstruirCadenaTurboAcelerada(sec_n2048.length, oraculo_turboA_n2048))

val oraculo_turboA_n4096 = crearOraculo(costoOraculo)(sec_n4096)
verificar("Prueba (TurboAcelerada) con n=4096", sec_n4096, reconstruirCadenaTurboAcelerada(sec_n4096.length, oraculo_turboA_n4096))


println("\n--- Fin de las pruebas ---")