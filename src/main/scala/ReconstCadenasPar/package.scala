import ArbolSufijos._
import Oraculo._
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

package object ReconstCadenasPar {

  // --- INGENUO PARALELO ---
  def reconstruirCadenaIngenuoPar(n: Int, o: Oraculo): Seq[Char] = {
    val candidatos: LazyList[Seq[Char]] =
      (1 to n).foldLeft(LazyList(Seq.empty[Char])) { (acc, _) =>
        for {
          prefijo <- acc
          c <- alfabeto
        } yield prefijo :+ c
      }

    // 2. Calcula el tamaño de los bloques para la división en 16 partes
    val numTareas = 16
    val totalCandidatos = math.pow(alfabeto.length, n).toInt
    val tamanoBloque = totalCandidatos / numTareas

    // 3. Lanza las 16 tareas en paralelo, anidando las llamadas a `common.parallel`.
    //    Cada llamada a `find` opera sobre su propio "slice" de la LazyList.
    val (res_0_3, res_4_7, res_8_11, res_12_15) = common.parallel(
      // Grupo de tareas 0 a 3
      common.parallel(
        candidatos.slice(0 * tamanoBloque, 1 * tamanoBloque).find(o),
        candidatos.slice(1 * tamanoBloque, 2 * tamanoBloque).find(o),
        candidatos.slice(2 * tamanoBloque, 3 * tamanoBloque).find(o),
        candidatos.slice(3 * tamanoBloque, 4 * tamanoBloque).find(o)
      ),
      // Grupo de tareas 4 a 7
      common.parallel(
        candidatos.slice(4 * tamanoBloque, 5 * tamanoBloque).find(o),
        candidatos.slice(5 * tamanoBloque, 6 * tamanoBloque).find(o),
        candidatos.slice(6 * tamanoBloque, 7 * tamanoBloque).find(o),
        candidatos.slice(7 * tamanoBloque, 8 * tamanoBloque).find(o)
      ),
      // Grupo de tareas 8 a 11
      common.parallel(
        candidatos.slice(8 * tamanoBloque, 9 * tamanoBloque).find(o),
        candidatos.slice(9 * tamanoBloque, 10 * tamanoBloque).find(o),
        candidatos.slice(10 * tamanoBloque, 11 * tamanoBloque).find(o),
        candidatos.slice(11 * tamanoBloque, 12 * tamanoBloque).find(o)
      ),
      // Grupo de tareas 12 a 15 (el último bloque va hasta el final)
      common.parallel(
        candidatos.slice(12 * tamanoBloque, 13 * tamanoBloque).find(o),
        candidatos.slice(13 * tamanoBloque, 14 * tamanoBloque).find(o),
        candidatos.slice(14 * tamanoBloque, 15 * tamanoBloque).find(o),
        candidatos.slice(15 * tamanoBloque, totalCandidatos).find(o)
      )
    )

    // 4. Desempaqueta los resultados de las tuplas anidadas
    val (r0, r1, r2, r3) = res_0_3
    val (r4, r5, r6, r7) = res_4_7
    val (r8, r9, r10, r11) = res_8_11
    val (r12, r13, r14, r15) = res_12_15

    // 5. Encadena la búsqueda del primer resultado válido (`Some`)
    r0.orElse(r1).orElse(r2).orElse(r3)
      .orElse(r4).orElse(r5).orElse(r6).orElse(r7)
      .orElse(r8).orElse(r9).orElse(r10).orElse(r11)
      .orElse(r12).orElse(r13).orElse(r14).orElse(r15)
      .getOrElse(Seq.empty[Char])
  }

  // --- MEJORADO PARALELO ---
  def reconstruirCadenaMejoradoPar(n: Int, o: Oraculo): Seq[Char] = {
    @tailrec
    def construirCandidatos(k: Int, scPrev: Seq[Seq[Char]]): Seq[Char] = {
      // paralelizamos sobre scPrev
      val scK: Seq[Seq[Char]] = (for {
        prefijo <- scPrev.par
        ch      <- alfabeto
        cand     = prefijo :+ ch
        if o(cand)
      } yield cand).toList

      scK.find(_.length == n) match {
        case Some(sol) => sol
        case None      => construirCandidatos(k + 1, scK)
      }
    }
    construirCandidatos(1, Seq(Vector.empty))
  }

  // --- TURBO PARALELO ---
  def reconstruirCadenaTurboPar(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0, "n debe ser potencia de dos > 0")

    @tailrec
    def construirCandidatos(k: Int, scH: Seq[Seq[Char]]): Seq[Char] = {
      // paralelizamos el producto cartesiano
      val candidatosK: Seq[Seq[Char]] =
        (for {
          s1 <- scH.par
          s2 <- scH
        } yield s1 ++ s2).toList

      val scK = candidatosK.par.filter(o).toList

      scK.find(_.length == n) match {
        case Some(sol) => sol
        case None      => construirCandidatos(k * 2, scK)
      }
    }

    val sc1 = alfabeto.map(ch => Seq(ch)).filter(o)
    if (n == 1) sc1.head else construirCandidatos(2, sc1)
  }

  // --- TURBO MEJORADO PARALELO ---
  def reconstruirCadenaTurboMejoradaPar(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0, "n debe ser potencia de dos > 0")

    def filtrar(scPrevSeq: Seq[Seq[Char]], scPrevSet: Set[Seq[Char]], half: Int): Seq[Seq[Char]] = {
      (for {
        s1 <- scPrevSeq.par
        s2 <- scPrevSeq
        comb = s1 ++ s2
        if comb.sliding(half).forall(sub => scPrevSet.contains(sub))
      } yield comb).toList
    }

    @tailrec
    def build(k: Int, scSet: Set[Seq[Char]]): Seq[Char] = {
      val scPrevSeq = scSet.toSeq
      val cand       = filtrar(scPrevSeq, scSet, k / 2)
      val scK        = cand.par.filter(o).toList

      scK.find(_.length == n) match {
        case Some(sol) => sol
        case None      => build(k * 2, scK.toSet)
      }
    }

    val base = alfabeto.map(ch => Seq(ch)).filter(o).toSet
    if (n == 1) base.head else build(2, base)
  }

  // --- TURBO ACELERADO PARALELO ---
  def reconstruirCadenaTurboAceleradaPar(n: Int, o: Oraculo): Seq[Char] = {
    require(n > 0 && (n & (n - 1)) == 0, "n debe ser potencia de dos > 0")

    def filtrarConTrie(scPrevSeq: Seq[Seq[Char]], trie: Trie, half: Int): Seq[Seq[Char]] = {
      (for {
        s1 <- scPrevSeq.par
        s2 <- scPrevSeq
        cand = s1 ++ s2
        if cand.sliding(half).forall(sub => ArbolSufijos.pertenece(sub, trie))
      } yield cand).toList
    }

    @tailrec
    def build(k: Int, scSeq: Seq[Seq[Char]], trie: Trie): Seq[Char] = {
      val cand  = filtrarConTrie(scSeq, trie, k / 2)
      val scK   = cand.par.filter(o).toList.distinct

      scK.find(_.length == n) match {
        case Some(sol) => sol
        case None =>
          if (scK.isEmpty)
            throw new RuntimeException(s"No hay subcadenas válidas de longitud $k")
          val nextTrie = ArbolSufijos.arbolDeSufijos(scK)
          build(k * 2, scK, nextTrie)
      }
    }

    val sc1 = alfabeto.map(ch => Seq(ch)).filter(o)
    if (sc1.isEmpty) throw new RuntimeException("No hay carácter inicial válido")
    if (n == 1) sc1.head
    else {
      val trie1 = ArbolSufijos.arbolDeSufijos(sc1)
      build(2, sc1, trie1)
    }
  }

}
