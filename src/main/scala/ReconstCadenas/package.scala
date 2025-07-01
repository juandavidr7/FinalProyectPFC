import ArbolSufijos.*
import Oraculo.*
import scala.annotation.tailrec


package object ReconstCadenas {
  // Solucion ingenua
  def reconstruirCadenaIngenuo(n: Int, o: Oraculo): Seq[Char] = {

    /**
     * Generador recursivo y perezoso de cadenas.
     * Es la versión funcional del backtracking.
     * @param k La longitud de las cadenas a generar.
     * @return Una LazyList de todos los posibles Vectores de longitud k.
     */
    def generarCadenas(k: Int): LazyList[Vector[Char]] = {
      // --- CASO BASE ---
      // Si pedimos cadenas de longitud 0, solo hay una: la vacía.
      if (k == 0) {
        LazyList(Vector.empty[Char])
      } else {
        // --- PASO RECURSIVO ---
        // 1. Obtenemos perezosamente todas las cadenas de la longitud anterior.
        val prefijos = generarCadenas(k - 1)

        // 2. Usamos for-comprehension para extender cada prefijo.
        //    'yield' produce los elementos de la nueva LazyList.
        for {
          prefijo <- prefijos   // Por cada prefijo de longitud k-1...
          char <- alfabeto      // ...y por cada carácter del alfabeto...
        } yield {
          // ...crea una nueva cadena de longitud k.
          // `prefijo :+ char` es EFICIENTE porque `prefijo` es un Vector.
          prefijo :+ char
        }
      }
    }

    // 1. Obtenemos la LazyList que representa a TODOS los candidatos posibles.
    //    Nada se ha calculado todavía, solo tenemos la "receta".
    val candidatos: LazyList[Vector[Char]] = generarCadenas(n)

    // 2. Buscamos el primer candidato que satisface el oráculo.
    //    Aquí es donde la generación perezosa ocurre, un candidato a la vez.
    val resultado: Option[Vector[Char]] = candidatos.find(o)

    // 3. Devolvemos el resultado. Vector es un Seq, por lo que no se necesita conversión.
    resultado.getOrElse(Seq.empty[Char])
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


  def reconstruirCadenaTurbo(n: Int, o: Oraculo): Seq[Char] = {
    // Verificar que n es potencia de 2
    require(n > 0 && (n & (n - 1)) == 0, "La longitud n debe ser una potencia de 2 y mayor que 0")

    @annotation.tailrec
    def construirCandidatos(k: Int, scPrev: Seq[Seq[Char]]): Seq[Char] = {
      // Generar el conjunto SCk concatenando subcadenas de longitud k/2
      val scK: Seq[Seq[Char]] =
        if (k == 1) {
          // Caso base: generar cadenas de longitud 1 desde el alfabeto
          Oraculo.alfabeto.map(Seq(_))
        } else {
          // Para k > 1: concatenar subcadenas de longitud k/2 con k/2
          val subcadenas = scPrev.filter(_.length == k / 2)
          for {
            s1 <- subcadenas // Primera subcadena de longitud k/2
            s2 <- subcadenas // Segunda subcadena de longitud k/2
            candidato = s1 ++ s2 // Concatenar s1 + s2
          } yield candidato
        }

      // Filtrar candidatos válidos consultando el oráculo
      val scKFiltrado = scK.filter(candidato => o(candidato))

      // Buscar si algún candidato ya alcanzó la longitud n
      scKFiltrado.filter(_.length == n) match {
        case sol :: _ => sol // Retornar la primera solución encontrada
        case Nil =>
          // No encontramos solución, continuar con el siguiente k
          val nuevoK = k * 2
          if (nuevoK > n) {
            throw new RuntimeException(s"No se pudo reconstruir la cadena de longitud $n")
          } else {
            // Pasar todas las subcadenas válidas encontradas hasta ahora
            construirCandidatos(nuevoK, scPrev ++ scKFiltrado)
          }
      }
    }

    // Comenzar con k=1 y un conjunto inicial que incluye todas las subcadenas válidas
    // de longitud 1 (que se generarán en la primera iteración)
    construirCandidatos(1, Seq.empty[Seq[Char]])
  }


  // Solucion turbo mejorada
  def reconstruirCadenaTurboMejorada(n: Int, o: Oraculo): Seq[Char] = {
    // Verificar que n sea potencia de 2 y mayor que 0
    require(n > 0 && (n & (n - 1)) == 0, "La longitud n debe ser una potencia de 2 y mayor que 0")

    def filtrar(scPrev: Seq[Seq[Char]], halfLen: Int): Seq[Seq[Char]] = {
      for {
        s1 <- scPrev
        s2 <- scPrev
        combined = s1 ++ s2

        // Verificar que cada subsecuencia contigua de longitud halfLen esté en scPrev
        subcadenas: Seq[Seq[Char]] = combined.sliding(halfLen).toSeq
        esValido = subcadenas.forall(sub => scPrev.contains(sub))
        if (esValido)
      } yield combined
    }

    @annotation.tailrec
    def build(k: Int, scPrev: Seq[Seq[Char]]): Seq[Char] = {
      if (k == 1) {
        // Nivel base: generar SC₁ = { Seq(c) | c ∈ alfabeto y o(Seq(c)) = true }
        val sc1 = Oraculo.alfabeto.map(ch => Seq(ch)).filter(o)
        if (sc1.isEmpty) {
          throw new NoSuchElementException(s"No existe carácter de longitud 1 que el oráculo acepte para construir S.")
        } else if (n == 1) {
          // Si n == 1, basta devolver el primer carácter válido
          sc1.head
        } else {
          // Pasar al siguiente nivel (k = 2) usando SC₁
          build(2, sc1)
        }
      } else {
        // k > 1: scPrev corresponde a SC_{k/2}
        // 1) Generar todos los posibles “s1 ++ s2” filtrados a priori
        val candidatos = filtrar(scPrev, k / 2)

        // 2) Preguntar al oráculo solo sobre esos candidatos
        val scK = candidatos.filter(o)

        // 3) Si hay alguna cadena de longitud n, devolverla
        scK.find(_.length == n) match {
          case Some(sol) =>
            sol

          case None =>
            // Si aún no llegamos a n, pasar al siguiente nivel (duplicar k)
            if (k * 2 > n) {
              // Ya no hay más niveles: no existe solución de longitud n
              throw new NoSuchElementException(s"No se pudo reconstruir la cadena de longitud $n")
            } else {
              // SC_{k} se convierte en scPrev del siguiente nivel
              build(k * 2, scK)
            }
        }
      }
    }
    // Iniciar con k = 1 y scPrev vacío (build los inicializará correctamente)
    build(1, Seq.empty)
  }



  // Version mejorada del algoritmo turbo que usa arboles de sufijos para acelerar el proceso
  def reconstruirCadenaTurboAcelerada(n: Int, o: Oraculo): Seq[Char] = {
    // Verificamos que n sea potencia de 2 y mayor que 0
    require(n > 0 && (n & (n - 1)) == 0, "La longitud n debe ser una potencia de 2 y mayor que 0")

    // Primero obtenemos todas las cadenas validas de longitud 1
    val subcadenasValidasLongitudUno = Oraculo.alfabeto.map(Seq(_)).filter(s => o(s)).toSeq

    // Si se busca una cadena de longitud 1 se devuelve la primera que encontramos
    if (n == 1) {
      if (subcadenasValidasLongitudUno.isEmpty) {
        throw new RuntimeException("No se encontro ninguna subcadena valida de longitud 1.")
      }
      return subcadenasValidasLongitudUno.head
    }

    // Creamos un arbol de sufijos con las cadenas de longitud 1 para optimizar busquedas
    val arbolSufijosInicial = ArbolSufijos.arbolDeSufijos(subcadenasValidasLongitudUno)

    // Funcion recursiva que va construyendo cadenas cada vez mas largas
    @annotation.tailrec
    def internal_turbo_acelerada(subcadenasValidasAnteriores: Seq[Seq[Char]], trieAnterior: ArbolSufijos.Trie, k: Int): Seq[Char] = {

      // Si k es mayor que n buscamos la solucion en las cadenas que ya tenemos
      if (k > n) {
        subcadenasValidasAnteriores.find(_.length == n) match {
          case Some(sol) => return sol
          case None => throw new RuntimeException(s"No se encontro solucion de longitud $n.")
        }
      }

      // Generamos nuevos candidatos combinando las subcadenas validas anteriores
      val candidatos = for {
        s1 <- subcadenasValidasAnteriores
        s2 <- subcadenasValidasAnteriores
      } yield s1 ++ s2

      // Comprobamos si ya tenemos la solucion entre los candidatos
      val solucionOpt = candidatos.find(cand => cand.length == n && o(cand))

      solucionOpt match {
        case Some(solucionEncontrada) => return solucionEncontrada
        case None => // No se encuentra la solucion se sigue con el algoritmo
      }

      // Filtramos los candidatos para quedarnos con las subcadenas validas de longitud k
      val nuevasSubcadenasValidas = candidatos.filter { cand =>
        cand.length == k && o(cand)
      }.foldLeft(Seq.empty[Seq[Char]]) { (acc, elem) =>
        if (acc.contains(elem)) acc else acc :+ elem
      } // Eliminamos duplicados con foldLeft

      // Si no hay subcadenas validas lanzamos una excepcion
      if (nuevasSubcadenasValidas.isEmpty) {
        throw new RuntimeException(s"No se encontraron subcadenas validas de longitud $k.")
      }

      // Creamos un nuevo arbol de sufijos con las subcadenas validas encontradas
      val nuevoTrie = ArbolSufijos.arbolDeSufijos(nuevasSubcadenasValidas)

      // Llamamos recursivamente a la funcion duplicando la longitud k
      internal_turbo_acelerada(nuevasSubcadenasValidas, nuevoTrie, k * 2)
    }

    // Iniciamos el proceso con las cadenas de longitud 1 y buscamos cadenas de longitud 2
    internal_turbo_acelerada(subcadenasValidasLongitudUno, arbolSufijosInicial, 2)
  }

}
