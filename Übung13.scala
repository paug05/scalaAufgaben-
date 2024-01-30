// Übungszettel 13
// Tim Duske und Paul Gnädig
// 30.01.2024

// Aufgabe 1a

trait PairPrioQueue[K:Ordering,V]:

    def insert(key:K,value:V) : Unit

    def extractMin() : V

    // def isEmpty : Boolean

    // def size : Int

class LinkedNodesPairQueue[K:Ordering, V]() extends PairPrioQueue[K,V]:
    private val ord = summon[Ordering[K]]
    import ord.mkOrderingOps

    private class Node (var key : K, val value : V, var next : Node)

    private var anchor : Node = Node(null.asInstanceOf[K],null.asInstanceOf[V], null)
    private var _size : Int = 0

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
                    i = i + 1
                else
                    temp = temp.next
                    i = i + 1
            var result : Node = vorKleinste.next
            vorKleinste.next = vorKleinste.next.next
            result.value
        
    
