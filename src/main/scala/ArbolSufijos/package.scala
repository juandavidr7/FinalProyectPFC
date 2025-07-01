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


  /**
   * Devuelve true si la secuencia s es reconocida por el trie t, y false si no.
   * La búsqueda asume que 't' es un nodo raíz (dummy) y la secuencia 's' se busca en sus hijos.
   * @param s La secuencia de caracteres a buscar.
   * @param t El trie en el que se busca.
   * @return Boolean indicando si la secuencia pertenece al trie.
   */
  def pertenece(s: Seq[Char], t: Trie): Boolean = {
    @annotation.tailrec
    def busquedaRec(sec: Seq[Char], trieActual: Trie): Boolean = {
      if (sec.isEmpty) {
        // Si ya consumimos toda la secuencia, devolvemos si el nodo final esta marcado.
        trieActual match {
          case Nodo(_, marcada, _) => marcada
          case Hoja(_, marcada)    => marcada
        }
      } else {
        trieActual match {
          case Hoja(_, _) => false // Si es hoja pero la secuencia sigue, ya no hay coincidencia.
          case Nodo(_, _, hijos) =>
            // Buscamos si hay un hijo con el caracter buscado.
            hijos.find(hijo => raiz(hijo) == sec.head) match {
              case Some(subTrie) => busquedaRec(sec.tail, subTrie)
              case None          => false
            }
        }
      }
    }
    // Arrancamos la busqueda en el trie con la secuencia dada.
    busquedaRec(s, t)
  }


  /**
   * Adiciona una secuencia de uno o más caracteres a un trie de forma inmutable.
   * @param s La secuencia a añadir.
   * @param t El trie al cual se añade la secuencia.
   * @return Un nuevo Trie con la secuencia añadida.
   */
  def adicionar(s: Seq[Char], t: Trie): Trie = {

    // Esta aux crea el camino de nodos u hojas para la nueva secuencia.
    def construirCamino(camino: Seq[Char]): Trie = camino.toList match {
      case h :: Nil => Hoja(h, marcada = true) // Si llega al ultimo caracter, marcamos hoja.
      case h :: r   => Nodo(h, marcada = false, List(construirCamino(r))) // Seguimos construyendo el camino.
      case Nil      => t // Este caso no deberia pasar.
    }

    if (s.isEmpty) {
      // Si recibimos vacio, solo marcamos el nodo actual.
      t match {
        case Nodo(c, _, h) => Nodo(c, marcada = true, h)
        case Hoja(c, _)    => Hoja(c, marcada = true)
      }
    } else {
      t match {
        // Si es una hoja pero hay que agregar mas, la pasamos a nodo con la rama que falta.
        case Hoja(c, m) => Nodo(c, m, List(construirCamino(s)))
        case Nodo(c, m, hijos) =>
          val charActual = s.head
          val restoSecuencia = s.tail

          hijos.find(hijo => raiz(hijo) == charActual) match {
            case Some(subTrie) =>
              // El hijo ya existe, agregamos lo que falta de la secuencia recursivamente.
              val nuevoSubTrie = adicionar(restoSecuencia, subTrie)
              // Actualizamos la lista de hijos reemplazando el viejo por el nuevo.
              val nuevosHijos = hijos.map(hijo => if (raiz(hijo) == charActual) nuevoSubTrie else hijo)
              Nodo(c, m, nuevosHijos)
            case None =>
              // Si el hijo no existe, creamos todo el camino y lo agregamos como rama nueva.
              val nuevaRama = construirCamino(s)
              Nodo(c, m, nuevaRama :: hijos)
          }
      }
    }
  }


  /**
   * Dada una secuencia no vacía de secuencias, devuelve el árbol de sufijos asociado.
   * @param ss La colección de secuencias (palabras) para construir el árbol.
   * @return El Trie que contiene todas las secuencias.
   */
  def arbolDeSufijos(ss: Seq[Seq[Char]]): Trie = {
    // 1. Creamos un nodo raíz con valor nulo
    val arbolVacio: Trie = Nodo(' ', marcada = false, List())

    // 2. Usamos foldLeft para iterar sobre cada secuencia y añadirla al arbol.
    // 'trieActual' es el arbol acumulado, y 'secuencia' es el siguiente elemento de 'ss'.
    ss.foldLeft(arbolVacio)((trieActual, secuencia) => adicionar(secuencia, trieActual))
  }
}
