import Oraculo._
import ReconstCadenas._

val secreto = List('g','a','a','t','c','c','a','g','a','t')
val oraculo = crearOraculo(0)(secreto)
val resultado = reconstruirCadenaIngenuo(secreto.length, oraculo)