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
  // Asegurarse de que la longitud no sea negativa
  val longitudValida = math.max(0, long)
  (1 to longitudValida).map(_ => alfabeto(random.nextInt(alfabeto.length)))
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

// =================================================================================
// INICIO DE LAS FUNCIONES IMPLEMENTADAS
// =================================================================================

/**
 * Genera secuencias de prueba de longitudes cortas y secuenciales (1, 2, 3, ... n).
 */
def secsCortasParaPruebas(n: Int): Seq[Seq[Char]] = for {
  i <- 1 to n
} yield secAlAzar(i)

/**
 * Genera secuencias de prueba de longitudes exponenciales (2, 4, 8, ... 2^n).
 */
def secsLargasParaPruebas(n: Int): Seq[Seq[Char]] = for {
  i <- 1 to n
} yield secAlAzar(math.pow(2, i).toInt)

/**
 * Ejecuta la reconstrucción ingenua para una colección de secuencias.
 * @return Una secuencia de tuplas (esperado, obtenido).
 */
def pruebasIngenuo(ss: Seq[Seq[Char]]): Seq[(Seq[Char], Seq[Char])] = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s, reconstruirCadenaIngenuo(s.length, o))

def pruebasIngenuo1(ss: Seq[Seq[Char]]): Seq[(Seq[Char], Seq[Char])] = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s, reconstruirCadenaIngenuo1(s.length, o))

/**
 * Ejecuta la reconstrucción mejorada para una colección de secuencias.
 * @return Una secuencia de tuplas (longitud, esperado, obtenido).
 */
def pruebasMejorado(ss: Seq[Seq[Char]]): Seq[(Int, Seq[Char], Seq[Char])] = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s, reconstruirCadenaMejorado(s.length, o))

/**
 * Ejecuta la reconstrucción turbo para una colección de secuencias.
 * @return Una secuencia de tuplas (longitud, esperado, obtenido).
 */
def pruebasTurbo(ss: Seq[Seq[Char]]): Seq[(Int, Seq[Char], Seq[Char])] = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s, reconstruirCadenaTurbo(s.length, o))

/**
 * Ejecuta la reconstrucción turbo mejorada para una colección de secuencias.
 * @return Una secuencia de tuplas (longitud, esperado, obtenido).
 */
def pruebasTurboMejorada(ss: Seq[Seq[Char]]): Seq[(Int, Seq[Char], Seq[Char])] = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s, reconstruirCadenaTurboMejorada(s.length, o))

/**
 * Ejecuta la reconstrucción turbo acelerada para una colección de secuencias.
 * @return Una secuencia de tuplas (longitud, esperado, obtenido).
 */
def pruebasTurboAcelerada(ss: Seq[Seq[Char]]): Seq[(Int, Seq[Char], Seq[Char])] = for {
  s <- ss
  o = crearOraculo(costoOraculo)(s)
} yield (s.length, s, reconstruirCadenaTurboAcelerada(s.length, o))

// =================================================================================
// FIN DE LAS FUNCIONES IMPLEMENTADAS
// =================================================================================


// ---------------------------------------------------------------------------------
// 2. GENERACIÓN DE DATOS DE PRUEBA
// ---------------------------------------------------------------------------------
println("Generando secuencias de prueba...")

// Secuencias para algoritmos lentos (n pequeño)
val secsCortas = secsCortasParaPruebas(14) // Genera secuencias de longitud 1 a 13

// Secuencias para algoritmos rápidos (n grande, potencias de 2)
// Genera secuencias de longitud 2, 4, 8, 16, 32, 64, ..., 4096
val secsLargas = secsLargasParaPruebas(12)

println("Datos generados. Iniciando pruebas...")


// =================================================================================
// 3. SUITES DE PRUEBAS PROGRESIVAS (Utilizando las nuevas funciones)
// =================================================================================

// ---------------------------------------------------------------------------------
// Pruebas para: reconstruirCadenaIngenuo
// Objetivo: Demostrar que funciona para n muy pequeños.
// ---------------------------------------------------------------------------------
println("\n--- Pruebas para reconstruirCadenaIngenuo ---")
val resultadosIngenuo = pruebasIngenuo(secsCortas)
for ((esperado, obtenido) <- resultadosIngenuo) {
  verificar(s"Prueba (Ingenuo) con n=${esperado.length}", esperado, obtenido)
}

val resultadosIngenuo1 = pruebasIngenuo1(secsCortas)
for ((esperado, obtenido) <- resultadosIngenuo) {
  verificar(s"Prueba (Ingenuo) con n=${esperado.length}", esperado, obtenido)
}

// ---------------------------------------------------------------------------------
// Pruebas para: reconstruirCadenaMejorado
// Objetivo: Demostrar que es una mejora, pero aún no es viable para n grandes.
// ---------------------------------------------------------------------------------
println("\n--- Pruebas para reconstruirCadenaMejorado ---")
// Usaremos un subconjunto de las secuencias largas para no tardar demasiado
val resultadosMejorado = pruebasMejorado(secsLargas.filter(_.length <= 32))
for ((len, esperado, obtenido) <- resultadosMejorado) {
  verificar(s"Prueba (Mejorado) con n=$len", esperado, obtenido)
}
println("-> 'Mejorado' es inviable para n > 32-64 debido a su complejidad espacial.")


// ---------------------------------------------------------------------------------
// Pruebas para: reconstruirCadenaTurbo y sus mejoras
// Objetivo: Demostrar su viabilidad y rendimiento en tamaños grandes.
// ---------------------------------------------------------------------------------
def ejecutarYVerificarPruebas(nombreAlgo: String, pruebasFunc: Seq[Seq[Char]] => Seq[(Int, Seq[Char], Seq[Char])], secuencias: Seq[Seq[Char]]): Unit = {
  println(s"\n--- Pruebas para reconstruirCadena$nombreAlgo ---")
  val resultados = pruebasFunc(secuencias)
  for ((len, esperado, obtenido) <- resultados) {
    verificar(s"Prueba ($nombreAlgo) con n=$len", esperado, obtenido)
  }
}

// Ejecutamos las pruebas para todas las versiones Turbo con las secuencias largas
ejecutarYVerificarPruebas("Turbo", pruebasTurbo, secsLargas)
ejecutarYVerificarPruebas("TurboMejorada", pruebasTurboMejorada, secsLargas)
ejecutarYVerificarPruebas("TurboAcelerada", pruebasTurboAcelerada, secsLargas)


println("\n--- Fin de las pruebas ---")