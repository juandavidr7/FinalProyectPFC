import Oraculo._
import ReconstCadenas._
import ReconstCadenasPar._

val secreto = List('g','a','a')
val oraculo = crearOraculo(1)(secreto)
val resultado1 = reconstruirCadenaIngenuo(secreto.length, oraculo)
val resultado1par = reconstruirCadenaIngenuoPar(5)(secreto.length, oraculo)
val resultado2 = reconstruirCadenaMejorado(secreto.length, oraculo)
val secreto2 = List('g','a','a','t','c','c','a','g')


val pruebaTurbo = reconstruirCadenaTurbo(secreto2.length, oraculo)
println("original: "+secreto2)
println("turbo: "+pruebaTurbo)