import ArbolSufijos._
import Oraculo._

package object ReconstCadenas {

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // Genera todas las cadenas de longitud exactamente n sobre el alfabeto
    val candidatos: Seq[Seq[Char]] =
      (1 to n).foldLeft(Seq(Seq.empty[Char])) { (acc, _) =>
        for {
          prefijo <- acc
          c <- alfabeto
        } yield prefijo :+ c
      }
    // Busca la primera cadena aceptada por el oráculo
    candidatos.find(o) match {
      case Some(cadena) => cadena
      case None => Seq.empty[Char]
    }
  }

  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {
    // Generar todas las secuencias válidas desde longitud 1 hasta n
    val todas: Seq[Seq[Char]] = (1 to n).foldLeft(Seq(Seq.empty[Char])) {
      (candidatosPrevios, _) =>
        for {
          prefijo <- candidatosPrevios
          c <- alfabeto
          nuevo = prefijo :+ c
          if o(nuevo)
        } yield nuevo
    }
    // Buscar una secuencia válida de longitud n
    todas.find(o) match {
      case Some(cadena) => cadena
      case None => throw new NoSuchElementException("¡No se encontró ninguna cadena aceptada por el oráculo!")
    }
  }

  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    // Comprobación de que la longitud es una potencia de 2 y positiva
    require(n > 0 && (n & (n - 1)) == 0, "La longitud n debe ser una potencia de 2 y mayor que 0")

    val alfabeto = Oraculo.alfabeto

    // Genera todas las cadenas posibles de longitud k combinando subcadenas de longitud k/2
    def generarConjuntos(k: Int): Seq[Seq[Char]] = {
      if (k == 0) Seq(Seq.empty[Char]) // Caso base 
      else if (k == 1) alfabeto.map(Seq(_))
      else {
        // k debe ser potencia de 2 y k >= 2
        val subcadenas = generarConjuntos(k / 2)
        for {
          a <- subcadenas
          b <- subcadenas
        } yield a ++ b
      }
    }

    // Función interna que reconstruye N-1 caracteres (S.substring(1))
    // k: tamaño del bloque actual a buscar
    // actual: la cadena construida hasta ahora
    def internal_turbo(k: Int, actual: Seq[Char]): Seq[Char] = {
      if (k == 0) { // Caso base: se intentaron bloques de N/2, N/4, ..., 1.
        actual // 'actual' ahora tiene N-1 caracteres.
      } else {
        val candidatas = generarConjuntos(k)
        // Encuentra un bloque 'b' tal que 'actual ++ b' sea una subcadena de S
        val bloqueEncontradoOpt = candidatas.find(b => o(actual ++ b))

        bloqueEncontradoOpt match {
          case Some(bloque) =>
            internal_turbo(k / 2, actual ++ bloque) // Siguiente bloque será de tamaño k/2
          case None =>
            throw new RuntimeException(s"No se encontró bloque válido de longitud $k para concatenar con '$actual'")
        }
      }
    }

    // --- Lógica principal de reconstruirCadenaTurbo ---

    if (n == 1) {
      // Caso especial: reconstruir una cadena de longitud 1
      val candidatasUnCaracter = generarConjuntos(1)
      candidatasUnCaracter.find(s => o(s)) match {
        case Some(charSeq) => charSeq
        case None => throw new RuntimeException("No se pudo reconstruir la cadena de longitud 1.")
      }
    } else {
      // Para n > 1 (y n es potencia de 2)
      // internal_turbo(n/2, Seq.empty[Char]) debería devolver S.substring(1), que tiene longitud n-1
      val cadenaSinPrimerCaracter = internal_turbo(n / 2, Seq.empty[Char])

      if (cadenaSinPrimerCaracter.length != n - 1) {
        // Esto indicaría que la premisa (que internal_turbo devuelve N-1 caracteres) es incorrecta.
        // O que la observación de que falta el primer caracter no siempre se cumple.
        throw new IllegalStateException(
          s"La función interna 'internal_turbo' devolvió una cadena de longitud ${cadenaSinPrimerCaracter.length}, " +
            s"se esperaba ${n - 1}. Cadena obtenida: '${cadenaSinPrimerCaracter.mkString}'. " +
            "Verifica la lógica de 'internal_turbo' o la observación del problema."
        )
      }

      // Ahora, encontramos el primer carácter que falta
      // Buscamos 'char' en el alfabeto tal que 'char' + cadenaSinPrimerCaracter sea una subcadena de S (y por ende S misma)
      val primerCaracterOpt = alfabeto.map(Seq(_)).find { charSeq =>
        o(charSeq ++ cadenaSinPrimerCaracter)
      }

      primerCaracterOpt match {
        case Some(primerChar) => primerChar ++ cadenaSinPrimerCaracter
        case None =>
          throw new RuntimeException(s"No se pudo encontrar el primer carácter para la cadena que termina en '${cadenaSinPrimerCaracter.mkString}'")
      }
    }
  }
  

  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa el filtro para ir más rápido
    // n debe ser potencia de 2
    ???
  }

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
    // Usa el filtro para ir más rápido
    // Usa árboles de sufijos para guardar Seq[Seq[Char]]
    // n debe ser potencia de 2
    ???
  }

}
