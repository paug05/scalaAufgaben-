// Übungszettel 14
// Tim Duske und Paul Gnädig

// Aufgabe 1a.) siehe abgegebene PDF

// Aufgabe 1b.) siehe between Methode im Quellcode

trait MyDict[K, V]:
  // In a dictionary we store pairs of keys of data type K
  // and values of data type V. For each key there is at most
  // one value.

  // Precondition: None
  // Effect: The dictionary contains key with the given value.
  def put(key: K, value: V): Unit

  // Precondition: None
  // Effect: The dictionary does not contain key.
  def remove(key: K): Unit

  // Precondition: The dictionary contains key.
  // Result: The corresponding value of key in the dictionary is returned.
  def get(key: K): V

  // Precondition: None
  // Result: True is returned, if and only if the dictionary contains key.
  def contains(key: K): Boolean

  // Precondition: None
  // Result: True is returned, if and only if the dictionary is empty.
  def isEmpty: Boolean

  def between(key1: K, key2: K): List[K]

  def height(): Int

  def isAVL() : Boolean

import scala.math._ // für die height Methode aus 1c.)

def isAVL(): Boolean

class BinarySearchTree[K: Ordering, V] extends MyDict[K, V]:
  private val ord = summon[Ordering[K]]
  import ord.mkOrderingOps

  private class Node(val key: K, var value: V, var left: Node, var right: Node)
  private var root: Node = null

  // empty tree:
  def isEmpty: Boolean = root == null

  // Searching in a binary search tree =======================================
  def get(key: K): V = search(root, key).value

  def contains(key: K): Boolean = search(root, key) != null

  // Precondition: None
  // Result: The node containing key is returned,
  // if key is in the binary search tree rooted at curr.
  // If key is not in the tree, null is returned.
  private def search(curr: Node, key: K): Node =
    if curr == null || curr.key == key then curr
    else if curr.key > key then search(curr.left, key) // go to the left subtree
    else // curr.key < key
      search(curr.right, key) // go to the right subtree

  // Inserting in a binary search tree =======================================
  def put(key: K, value: V): Unit =
    root = insert(root, key, value)

  // Precondition: Let curr be the root of a binary search tree T.
  // Effect: If T contains a node with key, then its value is updated.
  // If T does not contain key, a new node in T is inserted with
  // content (key, value).
  // Result: The root of the updated binary search tree is returned.
  private def insert(curr: Node, key: K, value: V): Node =
    if curr == null then // key is not contained: new leaf
      Node(key, value, null, null)
    else if curr.key == key then // key is contained: update
      curr.value = value
      curr
    else if curr.key > key then // insert in left subtree
      curr.left = insert(curr.left, key, value)
      curr
    else // curr.key < key       // insert in right subtree
      curr.right = insert(curr.right, key, value)
      curr

  // Removing in a binary search tree ========================================
  def remove(key: K): Unit =
    root = remove(root, key)

  // Precondition: Let curr be the root of a binary search tree T.
  // Effect: The binary search tree does not contain a node with key.
  // Result: The root of the updated binary search tree is returned.
  private def remove(curr: Node, key: K): Node =
    if curr == null then // key is not in the tree
      curr
    else if curr.key > key then // remove in left subtree
      curr.left = remove(curr.left, key)
      curr
    else if curr.key < key then // remove in right subtree
      curr.right = remove(curr.right, key)
      curr
    else if curr.left == null then // curr.key == key
      curr.right
    else // curr.left != null && curr.key == key
      // remove largest element in left subtree to update the
      // current node, that originally contains key
      val (k, v, left): (K, V, Node) = removeMax(curr.left)
      Node(k, v, left, curr.right)

  // Precondition: Let curr be the root of a non-empty binary search tree T.
  // Effect: The node with the largest key in T is removed.
  // Result: The root of the updated binary search tree is returned.
  // The largest key and its corresponding value is returned.
  private def removeMax(curr: Node): (K, V, Node) =
    if curr.right == null then // no right subtree => root = maximum
      (curr.key, curr.value, curr.left)
    else // otherwise find maximum in right subtree
      val (k, v, n) = removeMax(curr.right)
      curr.right = n
      (k, v, curr)

  private def dfs: List[K] =
    def preorder(curr: Node): List[K] =
      if curr == null then Nil
      else List(curr.key) ::: preorder(curr.left) ::: preorder(curr.right)
      // Use preorder starting at root
    preorder(root)

  // Voraussetzung: der Suchbaum darf nicht leer sein, sonst wird eine leere Liste übergeben
  // Ergebnis: alle Keys die zwihen k1 und k2 liegen sind in einer Liste geliefert
  // Effekt: kein Effekt
  def between(k1: K, k2: K): List[K] =
    var result: List[K] = List()
    for i <- 0 to dfs.length - 1 do // baum in sortierte Liste übertragen
      if dfs(i) > k1 && dfs(i) < k2 then result = result :+ dfs(i)
    return result

// Aufgabe 1c.)
  private def getHeight(curr: Node): Int =
    if curr == null then 0
    else 1 + max(getHeight(curr.right), getHeight(curr.left))

  def height(): Int =
    getHeight(root)
  
      
  private def isAVL(curr:Node):Boolean=
    if abs(getHeight(curr.left) - getHeight(curr.right)) <= 1 then
      true
    else
      false
    
  def isAVL() : Boolean=
    isAVL(root)

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
  println(tree.height())
  println(tree.between('A', 'F'))
  println(tree.isAVL())
