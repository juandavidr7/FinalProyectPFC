import ArbolSufijos.*
import Oraculo.*
import scala.annotation.tailrec


package object ReconstCadenas {

  // Solucion ingenua
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


  // Solucion mejorada
  def reconstruirCadenaMejorado(n: Int, o: Oraculo): Seq[Char] = {

    @annotation.tailrec
    def construirCandidatos(k: Int, scPrev: Seq[Seq[Char]]): Seq[Char] = {

      val scK: Seq[Seq[Char]] = for {
        prefijo <- scPrev // Toma cada subcadena válida de longitud k−1
        ch <- Oraculo.alfabeto // Prueba cada carácter posible del alfabeto
        candidato = prefijo :+ ch // Concatena el prefijo ++ ch para formar la nueva cadena
        if o(candidato) // Filtra sólo las que el oráculo (o) acepta
      } yield candidato // scK es la lista de todos los candidatas válidos

      // Busca si alguna candidato ya alcanzó longitud n
      scK.filter(_.length == n) match {
        case sol :: _ => sol
        case Nil => construirCandidatos(k + 1, scK)
      }
    }

    // Entrada: k = 1 (cadenas de longitud 1), scPrev = Seq(Seq.empty) (sólo la cadena vacía)
    construirCandidatos(1, Seq(Seq.empty[Char]))
  }


  // Solucion turbo
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    // Comprobación de que la longitud es una potencia de 2 y positiva
    require(n > 0 && (n & (n - 1)) == 0, "La longitud n debe ser una potencia de 2 y mayor que 0")

    val subcadenasValidasLongitudUno: Set[Seq[Char]] = Oraculo.alfabeto.map(Seq(_)).filter(s => o(s)).toSet

    if (n == 1) {
      // Si n es 1, y después de filtrar el alfabeto, no queda ninguna subcadena válida de longitud 1,
      // entonces algo está mal (o la cadena secreta no usa caracteres del alfabeto provisto).
      if (subcadenasValidasLongitudUno.isEmpty) {
        throw new RuntimeException("No se encontró ninguna subcadena válida de longitud 1 a partir del alfabeto. No se puede reconstruir.")
      }
      // Si no está vacío, cualquier elemento es la respuesta (ya que solo buscamos longitud 1).
      // .head tomará el primer (y en este caso, único esperado) elemento.
      return subcadenasValidasLongitudUno.head
    }

    // 'subcadenasValidasAnteriores' son las SCk/2
    @tailrec
    def internal_turbo(subcadenasValidasAnteriores: Set[Seq[Char]], k: Int): Seq[Char] = {

      if (k > n) {
        subcadenasValidasAnteriores.find(_.length == n) match {
          case Some(sol) => return sol
          case None => throw new RuntimeException("No se encontró solución")
        }
      }

      val candidatos = for {
        s1 <- subcadenasValidasAnteriores
        s2 <- subcadenasValidasAnteriores
      } yield s1 ++ s2

      // Primero, buscamos si la solución ya está en los candidatos.
      //option se usa para casos donde son soluciones opcionales, lo impolementé aquí para que no de errores en el bucle ni nada por el estilo
      val solucionOpt: Option[Seq[Char]] = candidatos.find { cadenaCandidata =>
        cadenaCandidata.length == n && o(cadenaCandidata)
      }

      if (solucionOpt.isDefined) {
        return solucionOpt.get // Si se encuentra la solución aquí pues se retorna inmediatamente, no como lo estaba haciendo antes xd
      }

      // Si no se encontró la solución final, filtramos los candidatos válidos para la siguiente iteración.
      //El Set sirve para eliminar cadenas duplicadas y hace todo más sencillo que si hubiera puesto Seq[Seq[Char]] porque tocaría hacer más comprobaciones
      val conjuntoNuevasSubcadenas: Set[Seq[Char]] = candidatos.filter { cadenaCandidata =>
        // Aseguramos que solo consideramos cadenas de la longitud actual K para la siguiente etapa, y que son válidas según el oráculo.
        cadenaCandidata.length == k && o(cadenaCandidata)
      }

      if (conjuntoNuevasSubcadenas.isEmpty) {
        throw new RuntimeException("No se encontraron subcadenas válidas de longitud k")
      }

      internal_turbo(conjuntoNuevasSubcadenas, k * 2)
    }

    internal_turbo(subcadenasValidasLongitudUno, 2)
  }


  // Solucion turbo mejorada
//  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
//    // Usa la propiedad de que si s = s1 ++ s2 entonces s1 y s2 también son subsecuencias de s
//    // Usa el filtro para ir más rápido
//    // n debe ser potencia de 2
//    ???
//  }


  // Version mejorada del algoritmo turbo que usa arboles de sufijos para acelerar el proceso

  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {

    // Verificamos que n sea potencia de 2 y mayor que 0

    require(n > 0 && (n & (n - 1)) == 0, "La longitud n debe ser una potencia de 2 y mayor que 0")


    // Primero obtenemos todas las cadenas validas de longitud 1 usando el oráculo

    val subcadenasValidasLongitudUno = Oraculo.alfabeto.map(Seq(_)).filter(s => o(s)).toSeq


    // Si se busca una cadena de longitud 1 retornamos la primera válida

    if (n == 1) {

      if (subcadenasValidasLongitudUno.isEmpty) {

        throw new RuntimeException("No se encontro ninguna subcadena valida de longitud 1.")

      }

      return subcadenasValidasLongitudUno.head

    }


    // Creamos un arbol de sufijos con las cadenas de longitud 1 para optimizar búsquedas

    val arbolActual = ArbolSufijos.arbolDeSufijos(subcadenasValidasLongitudUno)


    // Funcion recursiva que construye cadenas cada vez más largas usando doble longitud
    @annotation.tailrec
    def internal_turbo_acelerada(subcadenasValidasAnteriores: Seq[Seq[Char]], k: Int): Seq[Char] = {

      // Si la longitud actual supera n, buscamos la solución en las cadenas acumuladas

      if (k > n) {

        subcadenasValidasAnteriores.find(_.length == n) match {
          case Some(sol) => return sol
          case None => throw new RuntimeException(s"No se encontro solucion de longitud $n.")
        }

      }

      // Generamos nuevos candidatos combinando las subcadenas validas anteriores (s1 ++ s2)

      val candidatos = 
        for{
        s1 <- subcadenasValidasAnteriores
        s2 <- subcadenasValidasAnteriores
      } yield s1 ++ s2


      // Filtramos candidatos válidos usando el oráculo y eliminamos duplicados con el árbol de sufijos

      val (nuevasSubcadenas, nuevoArbol) = candidatos.foldLeft((Seq.empty[Seq[Char]], arbolActual)) { (acc, elem) =>
        val (validos, nuevoTrie) = acc
        // Verificamos longitud objetivo y validez por oráculo primero (costosa)
        if (elem.length == k && o(elem)) {
          // Usamos el árbol de sufijos para evitar duplicados (eficiente)

          if (ArbolSufijos.pertenece(elem, nuevoTrie)) (validos, nuevoTrie)

          else (validos :+ elem, ArbolSufijos.adicionar(elem, nuevoTrie)) // Solo agregamos si es nuevo

        } else {
          (validos, nuevoTrie) // Descartamos candidatos inválidos o de longitud incorrecta
        }

      }


      // Si no hay nuevas subcadenas válidas de longitud k, lanzamos error

      if (nuevasSubcadenas.isEmpty) {
        throw new RuntimeException(s"No se encontraron subcadenas validas de longitud $k.")
      }

      // Preparamos la próxima iteración con las nuevas subcadenas y el árbol actualizado
      internal_turbo_acelerada(nuevasSubcadenas, k * 2)

    }
    // Iniciamos el proceso con las cadenas de longitud 1 y buscamos hasta alcanzar n
    internal_turbo_acelerada(subcadenasValidasLongitudUno, k = 2)

  }

}
