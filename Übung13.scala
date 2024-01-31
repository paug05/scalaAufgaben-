// Übungszettel 13
// Paul Gnädig und Tim Duske
// 30.01.2024

// Aufgabe 1a ----------------------------------------------------------------------------------

trait PairPrioQueue[K:Ordering,V]:

    // Precondition: None
    // Result: None
    // Effect: (key,value) is now the most recent Node.
    def insert(key:K,value:V) : Unit

    // Precondition: PairPrioQueue is not empty.
    // Result: The value of the Node with the smallest key is returned.
    // Effect: The Node with the smallest key is removed.
    def extractMin() : V

    // Precondition: None
    // Result: None
    // Effect: true is returned if the Queue has no Nodes, except the dummy Node.
    def isEmpty : Boolean

    // Precondition: None
    // Result: None
    // Effect: The number of Nodes, except the dummy Node, of the PairPrioQueue is returned.
    def size : Int

    // Precondition: PairPrioQueue is not empty.
    // Result: The value of the first Node after the dummy Node of the Queue is returned.
    // Effect: None
    def top : V


// Aufgabe 1b ----------------------------------------------------------------------------------


class LinkedNodesPairQueue[K:Ordering, V]() extends PairPrioQueue[K,V]:
    private val ord = summon[Ordering[K]]
    import ord.mkOrderingOps

    private class Node (var key : K, val value : V, var next : Node)

    private var anchor : Node = Node(null.asInstanceOf[K],null.asInstanceOf[V], null)
    private var _size : Int = 0

    def isEmpty : Boolean = anchor.next == null

    def size : Int = _size

    def top : V = anchor.next.value

    def insert(key:K, value:V): Unit =
        val newNode : Node = Node(key,value,anchor.next)
        anchor.next = newNode
        _size = _size + 1
    
    def extractMin() : V =
        if anchor.next == null then throw new Exception("The Queue is empty.")
        else
            var temp : Node = anchor
            var vorKleinste : Node = anchor
            var i : Int = 1
            while i <= _size - 1 do
                if temp.next.key < temp.key then
                    vorKleinste = temp
                    temp = temp.next
                    i = i + 1
                else
                    temp = temp.next
                    i = i + 1
            var result : Node = vorKleinste.next
            vorKleinste.next = vorKleinste.next.next
            _size = _size - 1
            result.value

def test1b() =

    val n : PairPrioQueue[Int,Double] = LinkedNodesPairQueue[Int,Double]

    n.insert(4,0.123)
    n.insert(2,0.3456)
    n.insert(3,0.7644)

    println(n.extractMin())
    println(n.extractMin())
    println(n.extractMin())


// Aufgabe 1c ------------------------------------------------------------------------------------


trait MyStack[A]:
    // (I): Objects of an arbitrary but fixed type A are stored in a stack.

    // Precondition: None
    // Result: None
    // Effect: x is now the most recent element in the stack.
    def push(x : A) : Unit

    // Precondition: Stack is not empty.
    // Result: The most recent element is returned.
    // Effect: The most recent element is removed from the stack.
    def pop() : A

    // Precondition: Stack is not empty.
    // Result: The most recent element is returned.
    // Effect: None
    def top : A

    // Precondition: None
    // Result: None
    // Effect: true is returned if and only if the stack has no elements.
    def isEmpty : Boolean

    // Precondition: None
    // Result: None
    // Effect: The number of elements in the stack is returned.
    def size : Int


class LinkedNodesStack[A] extends MyStack[A]:

    private val pq : PairPrioQueue[Int,A] = LinkedNodesPairQueue[Int,A]

    def isEmpty : Boolean = pq.isEmpty

    def size : Int = pq.size

    def top : A =
        if !isEmpty then pq.top
        else throw new Exception("Stack ist leer!")

    def pop() : A =
        if !isEmpty then
            pq.extractMin()
        else throw new Exception("Stack ist leer!")

    private var temp : Int = 1

    def push(x:A) : Unit =
        pq.insert(-(pq.size),x)


def test1c()=
    val testStack : MyStack[Int] = LinkedNodesStack[Int]()
    for i <- 0 to 10 do
        testStack.push(i)
    while !testStack.isEmpty do
        println(testStack.pop())

// Aufgabe 2a -----------------------------------------------------------------------------------

// siehe PDF-Datei

/* Aufgabe 2b ------------------------------------------------------------------------------------

(1) 2 hoch i Elemente in Ebene i in einem Heap:

    Angenommen T ist ein Heap mit n Elementen und Höhe h, welcher die Ordnungseigenschaft und die Vollständigkeitseigenschaft erfüllt.
    Dann ist der Heap nach der Vollständigkeitseigenschaft auf jeder Ebene 0...h-1 volständig gefüllt. Somit hat der Heap auf Ebene 0 
    genau 1 Element also 2 hoch 0 Elemente. Da wir einen binären Heap betrachten hat jedes Element 2 Kinder (rechtes Kind und linkes 
    Kind). Somit kann die Anzahl der Elemente einer Ebene 0 <= i < h mit 2^i berechnet werden, da wir für jeden Knoten der darüber
    liegenden Ebene zwei Kinder in der betrachteten Ebene haben. Hat die obere Ebene also i Knoten, so hat die betrachtete Ebene
    2^i Kinder, bzw. Knoten.
    

(2) Auf Ebene h mindestens 1 und höchtens 2 hoch h Elemente 

    Angenommen T ist ein Heap mit n Elementen und Höhe h, welcher die Ordnungseigenschaft und die Vollständigkeitseigenschaft erfüllt.
    Die Ebene h spiegelt in diesem Fall die unterste Ebene wieder, dass heißt, das sie, nach der Volständigkeitseigenschaft und (1),
    maximal 2^h Elemente haben kann. Außerdem hat diese Ebene mindestens 1 Element, da sie sonst nicht existeren würde und die Ebene
    darüber die Höhe des Baumes darstellen würde. Aus diesen beiden Vehalten kann man die Aussage folgern das es auf der Ebene der 
    Höhe mindestens 1 und maximal 2^h Elemente gibt.

(3) siehe PDF-Datei

(4) siehe PDF-Datei

*/
// Aufgabe 2c ------------------------------------------------------------------------------------

trait MyPrioQueue[K: Ordering]:

    // Precondition: None
    // Result: None
    // Effect: (key) is now the most recent Node.
    def insert(key: K): Unit

    // Precondition: PairPrioQueue is not empty.
    // Result: The value of the Node with the smallest key is returned.
    // Effect: The Node with the smallest key is removed.
    def extractMin(): K

    // Precondition: None
    // Result: None
    // Effect: true is returned if the Queue has no Nodes, except the dummy Node.
    def isEmpty : Boolean


import scala.reflect.ClassTag

class TernärHeap[K:Ordering:ClassTag](capacity:Int) extends MyPrioQueue[K]:
    private val ord = summon[Ordering[K]]
    import ord.mkOrderingOps

    private val array : Array[K] = new Array[K](if capacity < 2 then 2 else capacity)
    private var last = -1

    private def parent(i:Int) = (i-1)/3
    private def rchild(i:Int) = (3*i)+1
    private def mchild(i:Int) = (3*i) +2
    private def lchild(i:Int) = (3*i) +3

    private def swap(i:Int,j:Int) : Unit =
        val temp = array(i)
        array(i) = array(j)
        array(j) = temp
    
    def isEmpty : Boolean = last == -1

    private def minThree(a:Array[K],i:Int,j:Int,k:Int) : Int =
        if array(i) < array(j) && array(i) < array(k) then
            i
        else if array(j) < array(i) && array(j) < array(k) then
            j
        else
            k
    
    private def bubbleUp(i:Int):Unit=
        if array(parent(i)) > array(i) then
            swap(i,parent(i))
            bubbleUp(parent(i))
    
    private def bubbleDown(i:Int):Unit=
        if lchild(i) <= last && array(lchild(i)) < array(i) || 
           mchild(i) <= last && array(mchild(i)) < array(i) ||
           rchild(i) <= last && array(rchild(i)) < array(i)
        then
            val child = minThree(array,lchild(i),mchild(i),rchild(i))
            swap(i,child)
            bubbleDown(child)
    
    def insert(key:K) :Unit=
        if last == array.size-1 then throw new Exception("The Queue is full")
        last = last + 1
        array(last) = key
        bubbleUp(last)

    def extractMin():K =
        if isEmpty then throw new Exception("The Queue is empty")
        val result = array(0)
        array(0) = null.asInstanceOf[K]
        swap(0,last)
        last = last-1
        bubbleDown(0)
        result

def test2c()=

    val t : MyPrioQueue[Int] = TernärHeap[Int](10)
    var i = 9
    while i >= 0 do
        t.insert(i)
        i = i -1
    for i <- 0 to 9 do
        println(t.extractMin())

// Aufgabe 2d ------------------------------------------------------------------------------------

/* 
Da wir durch die Anmerkung in der Aufgabe wissen, dass ein Sortieralgorithmus mindestens die laufzeit n * log n hat und bekannt ist
,dass diese mit Vergleichsoprationen arbeiten, kann man daraus Schlussfolgern, dass es keinen Algorithmus geben wird er beide Operationen
der Prio-Queue schneller durchführen kann.

1. Prio_Queue in der LinkedNodes Implementierung:
    Falls wir eine Prio-Queue über die Linked Nodes Implemntierung haben kann man nochmal zwischen sortiert un unsortiert unterscheiden
    Im sortierten Fall ist die Laufzeit vom insert O(n), da man die ganze Liste ablaufen muss. Beim extractMin haben wir die 
    Laufzeit O(1), da das Minimum an erster Stelle liegt. In einer unsortierten Queue sind die Laufzeien genau umgekehrt.

    => eine der beiden Methoden hat die Laufzeit O(n) und die andere O(1) 

2. Prio-Queue in der Heap Implementeirung:
    Durch die binären Abzweigungen kann beim insert eines neuen Elements in logarithmischer Laufzeit das Ende erreicht werden. extractMin
    ist hierbei auch logarithmisch, da das kleinste Element entweder direkt in der Wurzel oder im Blatt liegt. Im Fall eines Blattes
    wird die BubbleUp Methode mit logarithmischer Laufzeit aufgerufen

    => beide Operationen haben eine Laufzeit von O(log n) 

*/