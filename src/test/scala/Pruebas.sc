import Oraculo._
import ReconstCadenas._

val secreto = List('g','a','a','t','c','c','a','g','a','t')
val oraculo = crearOraculo(0)(secreto)
val secreto2 = List('g','a','a','t','c','c','a','g')


val pruebaTurbo = reconstruirCadenaTurbo(secreto2.length, oraculo)
println("original: "+secreto2)
println("turbo: "+pruebaTurbo)