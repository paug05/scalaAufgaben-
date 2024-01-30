// Übungszettel 13
// Tim Duske und Paul Gnädig
// 30.01.2024

// Aufgabe 1a

trait PairPrioQueue[K:Ordering,V]:

    def insert(key:K,value:V) : Unit

    def extractMin() : V

    def isEmpty : Boolean

    def size : Int

class LinkedNodesPairQueue[K:Ordering, V]() extends PairPrioQueue[K,V]:
    private val ord = summon[Ordering[K]]
    import ord.mkOrderingOps

    private class Node (key : K, value : V, next : Node)

    private var anchor : Node = Node(null.asInstanceOf[K],null.asInstanceOf[V], null)
    private var _size : Int = 0

    def insert(key:K, value:V): Unit=
        val newNode : Node = Node(key,value,anchor.next)
        anchor.next = newNode
        _size = _size + 1
    
    def extractMin() : V =
        var kleinste : Node = anchor.value
        var temp = anchor.next
        var tempHinten = anchor
        var i : Int = 1
        while i <= _size do 
            if temp.value < temp.next.value then 
                temp = temp
                tempHinten = tempHinten
            else if temp.value > temp.next.value then 
                temp = temp.next
                tempHinten = tempHinten.next
            else if temp.value = null then 
            i = i +1
        tempHinten.next = temp.next
        return temp.value 
        
    
