package object ArbolSufijos {

  // Definiendo otra estructura para manipular Seq[Seq[Char]]
  abstract class Trie

  case class Nodo(car: Char, marcada: Boolean, hijos: List[Trie]) extends Trie
  case class Hoja(car: Char, marcada: Boolean) extends Trie

  def raiz(t: Trie): Char = t match {
    case Nodo(c, _, _) => c
    case Hoja(c, _)    => c
  }

  def cabezas(t: Trie): Seq[Char] = t match {
    case Nodo(_, _, lt) => lt.map(t => raiz(t))
    case Hoja(c, _)      => Seq[Char](c)
  }

  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    // Devuelve true si la secuencia s es reconocida por el trie t, y false si no.
    ???
  }

  def adicionar(s: Seq[Char], t: Trie): Trie = {
    // Adiciona una secuencia de uno o más caracteres a un trie
    ???
  }

  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // Dada una secuencia no vacía de secuencias devuelve el árbol de sufijos asociado a esas secuencias
    ???
  }
}
