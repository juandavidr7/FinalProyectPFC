import Oraculo._
import ReconstCadenas._

val secreto = List('g','a','a','t','c','c','a','g','a','t')
val oraculo = crearOraculo(0)(secreto)
val resultado1 = reconstruirCadenaIngenuo(secreto.length, oraculo)
val resultado2 = reconstruirCadenaMejorado(secreto.length, oraculo)