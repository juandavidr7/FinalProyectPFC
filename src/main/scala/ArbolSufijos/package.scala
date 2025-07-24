package object ArbolSufijos {

 
  case class Trie(
                   hijos: Map[Char, Trie] = Map.empty,
                   esFinDePalabra: Boolean = false
                 )
  @annotation.tailrec
  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    if (s.isEmpty) {
      // Si la secuencia se ha consumido, el resultado depende de si el nodo actual
      // está marcado como el final de una palabra.
      t.esFinDePalabra
    } else {
      // Buscamos el carácter actual en los hijos del nodo actual.
      t.hijos.get(s.head) match {
        // Si el hijo existe, continuamos la búsqueda recursivamente con el resto de la secuencia.
        case Some(subTrie) => pertenece(s.tail, subTrie)
        // Si no existe, la secuencia no está en el Trie.
        case None => false
      }
    }
  }
  
  def adicionar(s: Seq[Char], t: Trie): Trie = {
    if (s.isEmpty) {
      // Si la secuencia se ha consumido, creamos una copia del nodo actual
      // pero marcándolo como final de palabra.
      t.copy(esFinDePalabra = true)
    } else {
      val charActual = s.head
      val restoSecuencia = s.tail

      // Obtenemos el sub-árbol para el carácter actual, o un Trie vacío si no existe.
      val subTrieExistente = t.hijos.getOrElse(charActual, Trie())

      // Adicionamos recursivamente el resto de la secuencia al sub-árbol.
      val nuevoSubTrie = adicionar(restoSecuencia, subTrieExistente)

      // Creamos una copia del Trie actual, actualizando el mapa de hijos
      // con el sub-árbol modificado.
      t.copy(hijos = t.hijos + (charActual -> nuevoSubTrie))
    }
  }
  
  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // Empezamos con un Trie vacío y vamos añadiendo cada secuencia
    // usando foldLeft para mantener la inmutabilidad.
    ss.foldLeft(Trie())((trieActual, secuencia) => adicionar(secuencia, trieActual))
  }
}