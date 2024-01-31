// Übungszettel 13
// Tim Duske und Paul Gnädig
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
            // println(result.key)
            // println(result.value)
            result.value

def test1b() =

    val n : PairPrioQueue[Int,Double] = LinkedNodesPairQueue[Int,Double]

    n.insert(4,0.123)
    n.insert(2,0.3456)
    n.insert(3,0.7644)

    n.extractMin()
    println(n.size)
    println(n.isEmpty)
    n.extractMin()
    n.extractMin()
    println(n.size)
    println(n.isEmpty)


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



// Aufgabe 2b 

/*

(1) 2 hoch i Elemente in Ebene i in einem Heap:

    Auf Ebene 0 hat ein Heap genau 1 Element (die Wurzel) also 2 hoch 0 Elemente. Da wir einen binären Heap betrachten hat jedes
    Element 2 Kinder (rechts und links). Durch dieses Verhalten wird beim herunterwandern in den Ebenen die Anzahl der Elemnte 
    immer verdoppelt. Da wir immer zwei Kinder haben und sich die Anzahl immer verdoppelt können wir für eine beliebige Ebene i 
    kleiner als die Höhe des Baumes (die unterste Ebene muss nicht vollständig befüllt sein), die Anzahl der Elemente durch 2 hoch i berechnen.

(2) auf Ebene h mindestens 1 und höchtens 2 hoch h Elemente 

    Ebene h spiegelt in diesem Fall die unterste Ebene wieder, dass heißt, das sie maximal 2 hoch h Elemnet haben darf (geht aus (1)hervor )
    da wir sonst zu viele Elemente in der Ebene hätten und h so nicht mehr die die Höhe repräsentiert (wäre sonst h+1).
    Außerdem hat diese Ebene mindestens 1 Element, da sie sonst nicht existeren würde (Höhe wäre sonst h-1).
    Aus diesen beiden Vehalten kann man die Aussage folgern das es auf der Ebene der Höhe mindestens 1 und maximal 2 hoch höhe 
    Elemnet gibt.

(3)


*/

// Aufgabe 2c ternärer Heap

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
        if array(i) <= array(j) && array(i) <= array(k) then
            i
        else if array(j) <= array(i) && array(j) <= array(k) then
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


// Aufgabe 2d: Wieso ist eine Vergelichs-Operation immer langsamer als die andere 

/* 
Bei einer Prio Queue mus zwische zwei Fällen untescheiden:

1. wir haben eine bereits geordnete Queue: 
    Beim einfügen (insert) eines neun Elements haben wir die Laufzeit O(n), da wir die komplette Queue abarbeiten 
    um die passende Stelle zu ermitteln. Bei der extractMin Methode habe nwir die konstante Laufzeit O(1), da das Minimum 
    immer an erster Stelle liegt.

2. wir haben keine geordnete Queue:
    Beim insert wird das neue Element einafch eingefügt => konstante Laufzeit von O(1). Bei extractMin muss  

*/