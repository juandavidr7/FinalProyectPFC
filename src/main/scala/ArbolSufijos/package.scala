package object ArbolSufijos {

  /**
   * Un diseño de Trie más robusto y estándar.
   * Un Trie es un mapa de caracteres a otros Tries (sub-árboles),
   * y un booleano que indica si este nodo marca el final de una palabra.
   */
  case class Trie(
                   hijos: Map[Char, Trie] = Map.empty,
                   esFinDePalabra: Boolean = false
                 )

  /**
   * Devuelve true si la secuencia 's' está contenida en el Trie 't'.
   * @param s La secuencia a buscar.
   * @param t El Trie donde buscar.
   * @return Boolean indicando si la secuencia pertenece al Trie.
   */
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

  /**
   * Adiciona una secuencia 's' a un Trie 't' de forma inmutable.
   * @param s La secuencia a añadir.
   * @param t El Trie al cual se añade la secuencia.
   * @return Un nuevo Trie con la secuencia añadida.
   */
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

  /**
   * Construye un Trie a partir de una colección de secuencias.
   * @param ss La colección de secuencias (palabras).
   * @return El Trie que contiene todas las secuencias.
   */
  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // Empezamos con un Trie vacío y vamos añadiendo cada secuencia
    // usando foldLeft para mantener la inmutabilidad.
    ss.foldLeft(Trie())((trieActual, secuencia) => adicionar(secuencia, trieActual))
  }
}