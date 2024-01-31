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