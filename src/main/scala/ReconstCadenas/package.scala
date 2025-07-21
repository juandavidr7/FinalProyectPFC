import ArbolSufijos.*
import Oraculo.*
import scala.annotation.tailrec


package object ReconstCadenas {
  // Solucion ingenua
  // Solucion ingenua
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

    //  def reconstruirCadenaIngenuo1(n: Int, o: Oraculo): Seq[Char] = {
//    // Genera todas las cadenas de longitud exactamente n sobre el alfabeto
//    val candidatos: Iterator[Seq[Char]] =
//      (1 to n).foldLeft(Iterator(Seq.empty[Char])) { (acc, _) =>
//        for {
//          prefijo <- acc
//          c <- alfabeto.iterator
//        } yield prefijo :+ c
//      }
//    // Busca la primera cadena aceptada por el oráculo
//    candidatos.find(o) match {
//      case Some(cadena) => cadena
//      case None => Seq.empty[Char]
//    }
//  }
    // Solucion mejorada
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


    // VERSIÓN CORREGIDA de reconstruirCadenaTurbo
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
//        if (k == n) {
//          scK.headOption.getOrElse(throw new RuntimeException(s"No se encontró solución de longitud $n"))
//        } else {
//          // 4. Llamada recursiva: scK se convierte en el conjunto de subcadenas para el siguiente nivel
//          construirCandidatos(k * 2, scK)
//        }
      }

      // Nivel base: Encontrar todas las subcadenas válidas de longitud 1
      val sc1 = alfabeto.map(Seq(_)).filter(o)
      if (n == 1) sc1.head
      else construirCandidatos(2, sc1)
    }


    // VERSIÓN CORREGIDA de reconstruirCadenaTurboMejorada
    def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
      require(n > 0 && (n & (n - 1)) == 0, "...")

      // La función filtrar ahora espera un Set para búsquedas eficientes
      def filtrar(scPrevSeq: Seq[Seq[Char]], scPrevSet: Set[Seq[Char]], halfLen: Int): Seq[Seq[Char]] = {
        for {
          s1 <- scPrevSeq
          s2 <- scPrevSeq
          combined = s1 ++ s2
          subcadenas = combined.sliding(halfLen).toSeq
          // ¡Esta comprobación ahora es O(1) en promedio!
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
      // Verificación de precondición
      require(n > 0 && (n & (n - 1)) == 0, "La longitud n debe ser una potencia de 2 y mayor que 0")

      /**
       * Filtra los candidatos usando el principio "a priori".
       * En lugar de un Set, ahora usa un Trie para verificar la validez de las subcadenas.
       *
       * @param scPrevSeq  Las subcadenas válidas del nivel anterior.
       * @param scPrevTrie El Trie que contiene las mismas subcadenas para búsqueda rápida.
       * @param halfLen    La longitud de las subcadenas a verificar (k/2).
       * @return Una secuencia de candidatos pre-filtrados.
       */
      def filtrarConTrie(scPrevSeq: Seq[Seq[Char]], scPrevTrie: Trie, halfLen: Int): Seq[Seq[Char]] = {
        for {
          s1 <- scPrevSeq
          s2 <- scPrevSeq
          candidato = s1 ++ s2
          // Genera todas las subcadenas de longitud halfLen que se superponen
          subcadenas = candidato.sliding(halfLen).toSeq
          // ¡La clave! Verifica que cada subcadena exista en el Trie.
          // Esta operación es muy eficiente.
          esValido = subcadenas.forall(sub => ArbolSufijos.pertenece(sub, scPrevTrie))
          if esValido
        } yield candidato
      }

      @annotation.tailrec
      def build(k: Int, scK_div_2_seq: Seq[Seq[Char]], scK_div_2_trie: Trie): Seq[Char] = {
        // 1. Generar candidatos pre-filtrados usando el Trie del nivel anterior.
        val candidatos = filtrarConTrie(scK_div_2_seq, scK_div_2_trie, k / 2)

        // 2. Preguntar al oráculo solo sobre los candidatos prometedores.
        // Usamos toSet para eliminar duplicados eficientemente antes de filtrar.
        val scK_seq = candidatos.toSet.toSeq.filter(o)

        // 3. Comprobar si hemos llegado a la solución.
        if (k == n) {
          scK_seq.headOption.getOrElse(throw new RuntimeException(s"No se pudo reconstruir la cadena de longitud $n"))
        } else {
          // Si no hay subcadenas válidas para el siguiente nivel, no hay solución.
          if (scK_seq.isEmpty) {
            throw new RuntimeException(s"No se encontraron subcadenas válidas de longitud $k para continuar.")
          }
          // 4. Construir el Trie para el siguiente nivel y llamar recursivamente.
          val scK_trie = ArbolSufijos.arbolDeSufijos(scK_seq)
          build(k * 2, scK_seq, scK_trie)
        }
      }

      // --- Punto de partida del algoritmo ---

      // Nivel base: Encontrar todas las subcadenas válidas de longitud 1.
      val sc1_seq = Oraculo.alfabeto.map(Seq(_)).filter(o)

      if (sc1_seq.isEmpty) {
        throw new RuntimeException("No existe ningún carácter válido para iniciar la reconstrucción.")
      }

      // Si n=1, ya hemos terminado.
      if (n == 1) {
        sc1_seq.head
      } else {
        // Construir el primer Trie con las cadenas de longitud 1.
        val sc1_trie = ArbolSufijos.arbolDeSufijos(sc1_seq)
        // Iniciar la construcción recursiva para encontrar cadenas de longitud 2.
        build(2, sc1_seq, sc1_trie)
      }
    }

  }