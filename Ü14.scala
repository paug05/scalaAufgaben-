// Übungszettel 14
// Paul Gnädig und Tim Duske
// 06.02.2024

trait MyStack[A]:
  // (I): Objects of an arbitrary but fixed type A are stored in a stack.

  // Precondition: None
  // Result: None
  // Effect: x is now the most recent element in the stack.
  def push(x: A): Unit

  // Precondition: Stack is not empty.
  // Result: The most recent element is returned.
  // Effect: The most recent element is removed from the stack.
  def pop(): A

  // Precondition: Stack is not empty.
  // Result: The most recent element is returned.
  // Effect: None
  def top: A

  // Precondition: None
  // Result: None
  // Effect: true is returned if and only if the stack has no elements.
  def isEmpty: Boolean

  // Precondition: None
  // Result: None
  // Effect: The number of elements in the stack is returned.
  def size: Int

class LinkedNodesStack[A] extends MyStack[A]:
  private class Node(val item: A, val next: Node)

  // Verwaltungskopf:
  private var topNode: Node = null
  private var _size: Int = 0

  def isEmpty: Boolean = topNode == null

  def size: Int = _size

  def top: A =
    if !isEmpty then topNode.item
    else throw new Exception("Stack ist leer!")

  def pop(): A =
    if !isEmpty then
      val result: A = topNode.item
      topNode = topNode.next // Garbage Collection
      _size = _size - 1
      result
    else throw new Exception("Stack ist leer!")

  def push(x: A): Unit =
    val node: Node = Node(x, topNode)
    topNode = node
    _size = _size + 1

// Aufgabe 1b,1c,1d -----------------------------------------------------------------------------------

trait MyDict[K, V]:
  // In a dictionary we store pairs of keys of data type K and values of data type V. For each
  // key there is at most one value.

  // Precondition: None
  // Effect: The dictionary contains key with the given value.
  def put(key: K, value: V): Unit

  // Precondition: None
  // Effect: The dictionary does not contain key.
  def remove(key: K): Unit

  // Precondition: The dictionary contains key.
  // Effect: The corresponding value of key in the dictionary is returned.
  def get(key: K): V

  // Precondition: None
  // Result: True is returned, if and only if the dictionary contains key.
  def contains(key: K): Boolean

  // Precondition: None
  // Effect: True is returned, if and only if the dictionary is empty.
  def isEmpty: Boolean

  // Voraussetzung: keine
  // Effekt: keine
  // Ergebnis: alle Elemente bei denen, der key zwischen k1 und k2 liegt sind in einer Liste geliefert
  def between(key1: K, key2: K): List[K]

  // Voraussetzung: keine
  // Effekt: keine
  // Ergebnis: die Höhe des Binären-Suchbaums ist geliefert
  def height(): Int

  // Voraussetzung: keine
  // Effekt: keine
  // Ergebnis: es ist true geliefert wenn es sich um einen AVL Baum handelt, ansonsten ist false geliefert
  def isAVL(): Boolean

  // Voraussetzung: keine
  // Effekt: keine
  // Ergebnis: die eingegebene Liste ist nach der Inorder Traversierung sortiert (aufsteigend)
  def dfs2: List[K]

  // Voraussetzung: keine
  // Effekt: keine
  // Ergebnis: der Suchbaum ist nach dem Levelorder mit Stack Implementierung traversiert und in einer Liste ausgegeben
  def bfs: List[K]

import scala.math._ // für die height Methode

// Precondition: None
// Effect: None
// Result: An empty dictionary is returned.
class BinarySearchTree[K: Ordering, V] extends MyDict[K, V]:
  private val ord = summon[Ordering[K]]
  import ord.mkOrderingOps

  private class Node(val key: K, var value: V, var left: Node, var right: Node)
  private var root: Node = null

  def isEmpty: Boolean = root == null

  def get(key: K): V = search(root, key).value

  def contains(key: K): Boolean = search(root, key) != null

  private def search(curr: Node, key: K): Node =
    if curr == null || curr.key == key then curr
    else if curr.key > key then search(curr.left, key)
    else search(curr.right, key)

  def put(key: K, value: V): Unit =
    root = insert(root, key, value)

    // Precondition: Let curr be the root of the binary search tree T.
    // Effect: If T contains a node with key, then its value is updated.
    //         If T does not contain key, a new node in T is inserted with content(key,value)
    // Result: The root of the updated binary search tree is returned.
  private def insert(curr: Node, key: K, value: V): Node =
    if curr == null then Node(key, value, null, null)
    else if curr.key == key then
      curr.value = value
      curr
    else if curr.key > key then
      curr.left = insert(curr.left, key, value)
      curr
    else
      curr.right = insert(curr.right, key, value)
      curr

  // Precondition: Let curr be the root of a non-empty binary search tree T.
  // Effect: The node with the largest key in T is removed.
  // Result: The root of the updated binary search tree is returned. The largest key and its
  //         corresponding value is returned.
  private def removeMax(curr: Node): (K, V, Node) =
    if curr.right == null then (curr.key, curr.value, curr.left)
    else
      val (k, v, n) = removeMax(curr.right)
      curr.right = n
      (k, v, curr)

  def remove(key: K): Unit =
    root = remove(root, key)

  // Precondition: Let curr be the root of a binary search tree T.
  // Effect: The binary search tree does not contain a node with key.
  // Result: The root of the updated binary search tree is returned.
  private def remove(curr: Node, key: K): Node =
    if curr == null then curr
    else if curr.key > key then
      curr.left = remove(curr.left, key)
      curr
    else if curr.key < key then
      curr.right = remove(curr.right, key)
      curr
    else if curr.left == null then curr.right
    else
      val (k, v, left): (K, V, Node) = removeMax(curr.left)
      Node(k, v, left, curr.right)

  private def dfs: List[K] =
    def preorder(curr: Node): List[K] =
      if curr == null then Nil
      else List(curr.key) ::: preorder(curr.left) ::: preorder(curr.right)
      // Use preorder starting at root
    preorder(root)

  // Aufgabe 1b:
  def between(k1: K, k2: K): List[K] =
    var result: List[K] = List()
    for i <- 0 to dfs.length - 1 do
      if dfs(i) > k1 && dfs(i) < k2 then result = result :+ dfs(i)
    return result

  // Aufgabe 1c:
  private def getHeight(curr: Node): Int =
    if curr == null then 0
    else 1 + max(getHeight(curr.right), getHeight(curr.left))

  def height(): Int =
    getHeight(root)

  // Aufgabe 1d:
  private def isAVL(curr: Node): Boolean =
    if abs(getHeight(curr.left) - getHeight(curr.right)) <= 1 then true
    else false

  def isAVL(): Boolean =
    isAVL(root)

  // Aufgabe 2b:

  // Spezifikation in dem Trait

  def dfs2: List[K] =
    def inorder(curr: Node): List[K] =
      if curr == null then Nil
      else inorder(curr.left) ::: List(curr.key) ::: inorder(curr.right)
    inorder(root)

  // Aufgabe 2c:

  // Spezifikation im Trait von MyDict

  def bfs: List[K] =
    val stack: MyStack[Node] = LinkedNodesStack[Node]()
    stack.push(root)
    var result: List[K] = List()
    while !stack.isEmpty do
      val curr = stack.pop()
      if curr != null then
        result = result ::: List(curr.key)
        stack.push(curr.right)
        stack.push(curr.left)
        // stack.push(curr.right)
    result

def test(): Unit =
  val tree = BinarySearchTree[Char, Int]()
  val dict: MyDict[Char, Int] = tree
  dict.put('D', 1)
  dict.put('A', 1)
  dict.put('C', 1)
  dict.put('E', 1)
  dict.put('B', 1)
  dict.put('G', 1)
  dict.put('F', 1)
  dict.put('H', 1)
  dict.put('I', 1)
  dict.put('X', 1)
  println(tree.between('A', 'F'))
  println(tree.height())
  println(tree.isAVL())
  println(tree.dfs2)
  println(tree.bfs)

// Aufgabe 2b -----------------------------------------------------------------------------------

// Voraussetzung: K stammt aus einem geordneten Universum.
// Effekt: Alle Elemente der Eingabeliste werden in einen Baum eingefügt.
// Ergebnis: Die aufsteigend sortierte Eingabeliste ist geliefert.

// def foo[K : Ordering](list : List[K]) : List[K] =
//     val t = BinarySearchTree[K,K]()
//     for x <- list do t.put(x,x)
//         t.dfs // hier wird inorder genutzt

// Korrektheit:

// Aufgabe 2c -----------------------------------------------------------------------------------

/*

Die neue Implementierung der levelorder Funktion mit einem Stack funktioniert nun genau so wie
die Tiefensuche, da der Stack im Gegensatz zur Queue nach dem LIFO-Prinzip funktioniert. Dadurch
sollte man diese Implementierung von leverorder auch nicht mehr Breitensuche nennen. Durch das
Tauschen des push-Vorgangs der beiden Unterbäume sind in der ausgegebenen Liste die Elemente
des linken Unterbaums vor den Elementen des rechten Unterbaums.

 */
