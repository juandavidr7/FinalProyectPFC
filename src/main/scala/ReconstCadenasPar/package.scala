import ArbolSufijos._
import Oraculo._
import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters._

package object ReconstCadenasPar {

  // --- INGENUO PARALELO ---
  def reconstruirCadenaIngenuoPar(n: Int, o: Oraculo): Seq[Char] = {
    if (n == 0) {
      return if (o(Seq.empty)) Seq.empty else throw new RuntimeException("No hay solución para n=0")
    }

    def buscarDesdePrefijo(prefijo: Seq[Char]): Option[Seq[Char]] = {
      def generarSufijos(k: Int): Iterator[Seq[Char]] = {
        if (k == 0) Iterator(Seq.empty)
        else for {
          suf <- generarSufijos(k - 1)
          c   <- alfabeto.iterator
        } yield suf :+ c
      }
      generarSufijos(n - prefijo.length)
        .map(sufijo => prefijo ++ sufijo)
        .find(o)
    }

    // probamos en paralelo los cuatro alfabetos iniciales
    val (resA, resC, resG, resT) = common.parallel(
      buscarDesdePrefijo(Seq('a')),
      buscarDesdePrefijo(Seq('c')),
      buscarDesdePrefijo(Seq('g')),
      buscarDesdePrefijo(Seq('t'))
    )

    resA.orElse(resC).orElse(resG).orElse(resT).getOrElse(Seq.empty)
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
