import Oraculo._
import ReconstCadenas._

val secreto = List('g','a','a','t','c','c','a','g','a','t')
val oraculo = crearOraculo(0)(secreto)


val pruebaTurbo = reconstruirCadenaTurbo(secreto.length, oraculo)
println("original: "+secreto)
println("turbo: "+resultado)