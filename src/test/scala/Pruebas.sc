import Oraculo._
import ReconstCadenas._
import ReconstCadenasPar._
import Benchmark._
import scala.collection.parallel.CollectionConverters._

// =================================================================================
// EJECUCIÓN DE LA PRUEBA
// =================================================================================

println("--- Iniciando Benchmark Fiel al Ejemplo de Prueba ---")

// 1. Definimos la cadena secreta y los parámetros
val cadenaSecreta = Seq('g','g','c','a','t','a','c', 'g', 'a', 't', 'a', 'c', 'a', 'c') //
val longitud = cadenaSecreta.length
val umbral = 1024

println(s"Probando con una cadena secreta de longitud: $longitud")

// 2. Creamos el oráculo a partir de la cadena secreta
val oraculoDePrueba = crearOraculo(0)(cadenaSecreta)

// 3. Medimos el tiempo de la versión secuencial
println("Midiendo algoritmo secuencial...")
val tiempoSec = medirTiempo {
  val resultado = reconstruirCadenaIngenuo(longitud, oraculoDePrueba)
  // Verificación opcional para asegurar que la prueba es válida
  assert(resultado == cadenaSecreta, "Error: El resultado secuencial no es correcto!")
}

// 4. Medimos el tiempo de la versión paralela
println("Midiendo algoritmo paralelo...")
val tiempoPar = medirTiempo {
  val resultado = reconstruirCadenaIngenuoPar(umbral)(longitud, oraculoDePrueba)
  // Verificación opcional
  assert(resultado == cadenaSecreta, "Error: El resultado paralelo no es correcto!")
}

// 5. Mostramos los resultados
val speedUp = if (tiempoPar > 0) tiempoSec / tiempoPar else 0.0
println("\n--- Resultados del Benchmark ---")
println(f"Tiempo Secuencial: $tiempoSec%.4f ms")
println(f"Tiempo Paralelo:   $tiempoPar%.4f ms")
println(f"Factor de Aceleración (SpeedUp): $speedUp%.2fx")
println("\n--- Benchmark Finalizado ---")



//// Pruebas solución ingenua
//val sec1 = List('g','g','c','a','t','a','c', 'g','t','a', 'a', 'g', 'c','a', 't') //15
//val or1 = crearOraculo(0)(sec1)
////val resultado1 = reconstruirCadenaIngenuo(sec1.length, or1)
//// resultado1 == sec1
//
//val resultado2
//= reconstruirCadenaIngenuoPar(sec1.length)(sec1.length, or1)
//resultado2 == sec1

//val sec10 = List('a','t','c','g','g','a','t','c','t','a','g','c','a','g','t','c')
//val or10 = crearOraculo(1)(sec10)
//val resultado10 = reconstruirCadenaIngenuo(sec10.length, or10)
//resultado10 == sec10
// Aparece este error cuando lo intento probar: java.lang.OutOfMemoryError: Java heap space: failed reallocation of scalar replaced objects.
// Por esto lo voy a comentar


// Pruebas solución ingenua paralela
//val sec11 = List()
//val or11 = crearOraculo(1)(sec11)
//val resultado11 = reconstruirCadenaIngenuoPar(4)(sec11.length, or11)
//resultado11 == sec11
//
//val sec12 = List()
//val or12 = crearOraculo(1)(sec12)
//val resultado12 = reconstruirCadenaIngenuoPar(8)(sec12.length, or12)
//resultado12 == sec12
// Falta la implementacion, pero ya puse la estructura de las pruebas

//// Pruebas solución mejorada
//val sec1 = List('c','a','g','t','t','g','a','c')
//val or1 = crearOraculo(1)(sec1)
//val resultado1 = reconstruirCadenaMejorado(sec1.length, or1)
//resultado1 == sec1
//
//val sec2 = List('t','g','a','c','a','t','g','g','c','c','t','a','g','t','a','c')
//val or2 = crearOraculo(1)(sec2)
//val resultado2 = reconstruirCadenaMejorado(sec2.length, or2)
//resultado2 == sec2
//
//
//// Pruebas solución mejorada paralela
//val sec3 = List('g','a','t','c','c','a','g','t')
//val or3 = crearOraculo(1)(sec3)
//val resultado3 = reconstruirCadenaMejoradoPar(4)(sec3.length, or3)
//resultado3 == sec3
//
//val sec8 = List('a','g','c','t','t','a','g','c','g','g','a','c','t','c','a','t')
//val or8 = crearOraculo(1)(sec8)
//val resultado8 = reconstruirCadenaMejoradoPar(8)(sec8.length, or8)
//resultado8 == sec8
//
//
//// Pruebas solución turbo
//val sec4 = List('g','a','t','c','c','a','g','a')
//val or4 = crearOraculo(1)(sec4)
//val resultado4 = reconstruirCadenaTurbo(sec4.length, or4)
//resultado4 == sec4
//
//val sec6 = List('a','g','t','t','c','c','a','g','g','a','c','t','a','t','g','c')
//val or6 = crearOraculo(1)(sec6)
//val resultado6 = reconstruirCadenaTurbo(sec6.length, or6)
//resultado6 == sec6
//
//
////Pruebas solución turbo paralela
//val sec5 = List('a','t','g','g','c','a','a','t')
//val or5 = crearOraculo(1)(sec5)
//val resultado5 = reconstruirCadenaTurboPar(4)(sec5.length, or5)
//resultado5 == sec5
//
//val sec7 = List('g','c','a','t','a','g','g','t','t','c','a','a','g','t','c','c')
//val or7 = crearOraculo(1)(sec7)
//val resultado7 = reconstruirCadenaTurboPar(8)(sec7.length, or7)
//resultado7 == sec7
//
//
//// Pruebas solución turbo acelerada
//val sec12 = List('g', 'a', 't', 'c', 'a', 'a', 'g', 'a')
//val or12 = crearOraculo(1)(sec12)
//val resultado12 = reconstruirCadenaTurboAcelerada(sec12.length, or12)
//resultado12 == sec12
//
//val sec14 = List('a','g','t','t','c','c','a','g','g','a','c','t','a','t','g','c')
//val or14 = crearOraculo(1)(sec14)
//val resultado14 = reconstruirCadenaTurboAcelerada(sec14.length, or14)
//resultado14 == sec14
