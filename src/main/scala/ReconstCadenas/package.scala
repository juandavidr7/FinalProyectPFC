import ArbolSufijos.*
import Oraculo.*
import scala.annotation.tailrec


package object ReconstCadenas {

  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {
    // Genera todas las cadenas de longitud exactamente n sobre el alfabeto
    val candidatos: LazyList[Seq[Char]] =
      (1 to n).foldLeft(LazyList(Seq.empty[Char])) { (acc, _) =>
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

    @annotation.tailrec
    def construirCandidatos(k: Int, scPrev: Seq[Seq[Char]]): Seq[Char] = {

      val scK: Seq[Seq[Char]] = for {
        prefijo <- scPrev // Toma cada subcadena válida de longitud k−1
        ch <- alfabeto // Prueba cada carácter posible del alfabeto
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
  
  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0, "...")

    @annotation.tailrec
    def construirCandidatos(k: Int, scK_div_2: Seq[Seq[Char]]): Seq[Char] = {
      // 1. Generar candidatos a partir de las subcadenas del nivel anterior (k/2)
      val candidatosK = for {
        s1 <- scK_div_2
        s2 <- scK_div_2
      } yield s1 ++ s2

      // 2. Filtrar con el oráculo
      val scK = candidatosK.filter(o)

      // 3. Comprobar si hemos encontrado la solución
      scK.filter(_.length == n) match {
        case sol :: _ => sol
        case Nil => construirCandidatos(2*k, scK)
      }
    }

    // Nivel base: Encontrar todas las subcadenas válidas de longitud 1
    val sc1 = alfabeto.map(Seq(_)).filter(o)
    if (n == 1) sc1.head
    else construirCandidatos(2, sc1)
  }

  
  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0, "...")

    // La función filtrar ahora espera un Set para búsquedas eficientes
    def filtrar(scPrevSeq: Seq[Seq[Char]], scPrevSet: Set[Seq[Char]], halfLen: Int): Seq[Seq[Char]] = {
      for {
        s1 <- scPrevSeq
        s2 <- scPrevSeq
        combined = s1 ++ s2
        subcadenas = combined.sliding(halfLen).toSeq
        esValido = subcadenas.forall(sub => scPrevSet.contains(sub))
        if esValido
      } yield combined
    }

    @annotation.tailrec
    def build(k: Int, scK_div_2: Set[Seq[Char]]): Seq[Char] = {
      val scPrevSeq = scK_div_2.toSeq
      val candidatos = filtrar(scPrevSeq, scK_div_2, k / 2)
      val scK = candidatos.filter(o)

      if (k == n) {
        scK.headOption.getOrElse(throw new RuntimeException(s"No se encontró solución de longitud $n"))
      } else {
        build(k * 2, scK.toSet)
      }
    }

    val sc1 = alfabeto.map(Seq(_)).filter(o).toSet
    if (n == 1) sc1.head
    else build(2, sc1)
  }


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