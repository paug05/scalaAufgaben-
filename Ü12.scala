// Paul Gnädig, Tim Duske
// 23.01.2024
// Übungszettel 12

/* Aufgabe 1a

Bei dem Entwerfen von Scala könnte auf die Mehrfachvererbung verzichtet worden sein, da 
ein Vererbungsdiagramm mit Mehrfachvererbung sehr schnell sehr kompliziert und unübersichtlich
werden kann. Dementsprechend würde auch der Quellcode sehr komplex werden, weswegen der Einsatz
von Traits deutlich einfacher ist.

*/

// -------------------------------------------------------------------------------------------------------------------------------

// Aufgabe 1b

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



// Funktion zum testen der Korrektheit eines Klammerausdrucks:

def klammerPrüf(s: String) : Boolean =
    var stack : MyStack[Char] = DynamicArrayStack[Char]
    for i <- 0 to (s.length)-1 do
        if s(i) == '(' || s(i) == '[' || s(i) == '{' then
            stack.push(s(i))        // jede geöffnete Klammer wird auf den Stapel gepusht
        else if s(i) == ')' then    
            if stack.top == '(' then    // wenn die zuletzt geöffnete Klammer zu der geschlossenen
                stack.pop()             // passt, wird die geöffnete Klammer mit pop() entfernt
            else false
        else if s(i) == ']' then
            if stack.top == '[' then    // wenn die zuletzt geöffnete Klammer zu der geschlossenen
                stack.pop()             // passt, wird die geöffnete Klammer mit pop() entfernt
            else false
        else
            if stack.top == '{' then    // wenn die zuletzt geöffnete Klammer zu der geschlossenen
                stack.pop()             // passt, wird die geöffnete Klammer mit pop() entfernt
            else false
    stack.size == 0                     // wenn alle geöffneten Klammer richtig geschlossen wurden
                                        // ist die Größe des Stacks 0 und true ist geliefert
    

// --------------------------------------------------------------------------------------------------------------------------------

// Aufgabe 1c

trait MyStackMulti[A]:
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

    // Precondition: None
    // Result: None
    // Effect: All Elements of l are pushed on to the Stack.
    def multipush(l : List[A]) : Unit



class LinkedNodesStack[A] extends MyStackMulti[A]:
    private class Node(var item : A, val next :Node)

    // Verwaltungskopf:
    private var topNode : Node = null
    private var _size : Int = 0

    def isEmpty : Boolean = topNode == null

    def size : Int = _size

    def top : A =
        if !isEmpty then topNode.item
        else throw new Exception("Stack ist leer!")

    def pop() : A =
        if !isEmpty then
            val result : A = topNode.item
            topNode = topNode.next      // Garbage Collection
            _size = _size - 1
            result
        else throw new Exception("Stack ist leer!")

    def push(x:A) : Unit =
        val node : Node = Node(x,topNode)
        topNode = node
        _size = _size + 1
    
    def multipush(l:List[A]) : Unit =
        for i <- 0 to (l.length) - 1 do             // alle Elemente der Liste werden gepusht
            val node : Node = Node(l(i),topNode)
            topNode = node
            _size = _size + 1


@main
def test1c_LinkedNotes() : Unit =
    val testStack : MyStackMulti[Int] = LinkedNodesStack[Int]()
    testStack.multipush(List(1,2,3,4,5))
    while !testStack.isEmpty do
        println(testStack.pop())
        



import scala.reflect.ClassTag   // für Array mit beliebigen Datentypen

// Vorraussetzung: None
// Effekt: None
// Ergebnis: Ein leerer Stack mit Kapazität max(1,capacity) ist geliefert.
class ArrayStack[A:ClassTag](capacity:Int) extends MyStackMulti[A]:
    // Verwaltungskopf:
    private val array : Array[A]= new Array[A](if capacity < 1 then 1 else capacity)
    private var amount : Int = 0

    def isEmpty : Boolean = amount == 0
    def size : Int = amount
    def top : A =
        if !isEmpty then array(amount-1)
        else throw new Exception("Stack ist leer!")
    
    def push(x:A) : Unit =
        if amount <= array.length -1 then
            array(amount) = x
            amount = amount + 1
        else throw new Exception("Stack ist voll!")
    
    def pop() : A =
        if !isEmpty then
            val result : A = array(amount-1)
            array(amount-1) = null.asInstanceOf[A]
            amount = amount - 1
            result
        else throw new Exception ("Stack ist leer!")
    
    def multipush(l:List[A]) : Unit =
        var i : Int = 0
        while amount <= (array.length) - 1 && i <= (l.length) - 1 do    // alle Elemente der Liste werden gepusht
            array(amount) = l(i)
            amount = amount + 1
            i = i + 1

def test1c_Array() : Unit =
    val testStack : MyStackMulti[Int] = ArrayStack[Int](11)
        testStack.multipush(List(1,2,3,4,5,6,7,8,9,10))
    while !testStack.isEmpty do
        println(testStack.pop())

// --------------------------------------------------------------------------------------------------------------------------------

// Aufgabe 1d

import scala.reflect.ClassTag

class DynamicArrayStack[A : ClassTag] extends MyStack[A]:

    private var array : Array[A] = new Array[A](1)
    private var amount : Int = 0

    private def resize() : Unit =
        val cap : Int = array.length
        if cap/8 <= amount && amount < cap then return      // veränderte INV

        val newArray : Array[A] = new Array[A](if cap/8 > amount then cap/4 else cap*4) // veränderte Grenzen

        for i <- 0 to amount-1 do
            newArray(i) = array(i)
        array = newArray

    def push(x:A) : Unit =
        array(amount) = x
        amount = amount + 1
        resize()
    
    def pop() : A =
        if !isEmpty then
            val result : A = array(amount-1)
            array(amount -1) = null.asInstanceOf[A]
            amount = amount - 1
            resize()
            result
        else throw new Exception("Stack is empty")
    
    def top : A =
        if !isEmpty then array(amount-1)
        else throw new Exception("Stack is empty")

    def isEmpty : Boolean = amount == 0
    def size : Int = amount

def test1d(): Unit = 
    var testStack : MyStack[Int] = new DynamicArrayStack[Int]
    for i <- 0 to 50 do
        testStack.push(i)
    while !testStack.isEmpty do 
        println(testStack.pop())


/*  Vergleich der beiden Implementierungen von DynamicArrayStack:

1. Variante (resize mit Verdoppelung und Halbierung):
    Vorteil:
        - weniger nicht verbrauchter Speicher im Array, da nur verdoppelt, bzw. halbiert wird
    Nachteil:
        - mehr Zeitaufwand, da öfter ein neues Array erschaffen werden muss

2. Variante (resize mit Vervierfachung und Vierteln):
    Vorteil:
        - weniger Zeitaufwand, da seltener ein neues Array erschaffen werden muss
    Nachteil:
        - mehr nicht verbrauchter Speicher, da vervierfacht, bzw. geviertelt wird

Allgemein kann man sagen, dass beide Varianten ihre Vor- und Nachteile haben, jedoch kann der Zeitaufwand im Gegensatz zum
nicht verbrauchten Speicher sehr gering ausfallen, weswegen die erste Variante vermutlich effizienter wäre.

*/

// ------------------------------------------------------------------------------------------------------------------------------

/* Aufgabe 2a

(a) Überladen von Funktionen:

    Überladen von Funktionen bedeutet, dass derselbe Funktionsname mehrfach in einer Klasse
    benutzt werden kann. Dabei muss jedoch entweder die Anzahl der Übergabeparameter anders sein
    als bei den anderen gleichnamigen Funktionen oder der Datentyp eines Übergabeparameters ist
    anders als in den gleichnamigen Funktionen.

    Beispiel:

    def vergleich(wert1:Int,wert2:Int) : Int =
        if wert1 >= wert2 then
            wert1
        else wert2
    
    def vergleich(wert1:Int,wert2:Int,wert3:Int) : Int =
        if wert1 >= wert2 && wert1 >= wert3 then
            wert1
        else if wert2 >= wert1 && wert2 >= wert3 then
            wert2
        else wert3
    
(b) Coercion:

    Scala unterstützt Coercion, was heißt das man einen Datentypen in einen anderen Datentypen umwandeln kann.
    Dafür gibt es verschiedene Wege in Scala, wie die Promotion eines Datentypen zu einem anderen, mit dem Befehl 
    asInstanceOf[] und mit Pattern Matching, welche alle ihre Vor- und Nachteile haben.
    
    (1) Beispiel:

    var b : Byte = 5
    var c : Short = b
    var d : Int = c

    (2) Beispiel:

    class X: ...
    val f : X = ...
    class Y: ...
    val a : Y = f.asInstanceOf[X]

(c) Parametrischer Polymorphismus:

    Durch den parametrischen Polymorphismus ist es möglich eine Funktion, bzw. eine Klasse so zu definieren, dass sie auf jeden
    beliebigen Datentypen angewendet werden kann.

    Beispiel:

    def wert[A](wert:A) : A =
        var l : List[A] = []
        l :+ wert

(d) Inklusionspolymorphie:

    Einer Variable vom Typ A, kann ein Objekt vom Typ B zugeordnet werden, falls B eine
    Unterklasse von A ist.

    Beispiel: 
    
    class Person : ...
    class Studierender extends Person : ...
    class Prof extends Person : ...

    var p : Person = Studierender(...)

    p = Professor(...)

    Override:

    Kommt dieselbe Funktion in zwei Klassen vor, bei der eine Klasse von der anderen erbt, dann muss die Funktion der Unterklasse
    mit override die Funktion der Oberklasse überschreiben. So wird es möglich die selbe Funktion für mehrere Klassen zu schreiben,
    wobei die Funktion je nach Klasse unterschiedlich implementiert ist.

    Beispiel:

    class Arbeitender : ...
        def work() : Unit = ...
    class Informatiker extends Arbeitender : ...
        override def work() : Unit = ...

*/

// --------------------------------------------------------------------------------------------------------------------------------

// Aufgabe 2b

trait MyQueue[A]:
    def enqueue(x:A):Unit
    def dequeue():A
    def isEmpty : Boolean
    def size : Int

import scala.reflect.ClassTag

// Precondition: None
// Effect: None
// Result: An empty stack of maximum capacity max(1, capacity) is returned.
class DynamicArrayQueue[A : ClassTag] extends MyQueue[A]:
    // The header:
    private var array : Array[A] = new Array[A](2)
    private var n : Int = 2
    private var front : Int = 0
    private var back : Int = 0

    // Precondition: None
    // Result: None
    // Effect: (newBack != front && array.length/4 < (n+back-front)%n (size of the Array)) holds.
    private def resize() : Unit =
        var cap : Int = array.length
        var newBack : Int = (back+1) % n
        if newBack != front && cap/4 <= ((n+back-front)%n)-1 then return // do nothing if INV holds
        var temp : Int = 0

        // allocate a new array with double the size of the old array if the queue is full:
        if newBack == front then

            var newArray : Array[A] = new Array[A](cap*2)
            n = newArray.length

            // Copy all elements to new array:
            for i <- front to back do
                newArray(temp) = array(i)
                temp = temp + 1

            array = newArray // the old array is now removed
            println("Das neue Array sieht jetzt wie folgt aus: ")
            val l : List[A] = array.toList
            println(l)

        else    // allocate a new array with half the size of the old array if the queue is too empty:
            if array.length <= 2 then return
            else
                var newArray : Array[A] = new Array[A](cap/2)
                n = newArray.length

                // Copy all elements to new array:
                for i <- front to back do
                    newArray(temp) = array(i)
                    temp = temp + 1

                array = newArray // the old array is now removed
                back = ((n+back-front) % n)
                front = 0
                println("Das neue Array sieht wie folgt aus: ")
                val l : List[A] = array.toList
                println(l)



    def enqueue(x:A) : Unit =
        // The next position after back
        // in the cycling ordering:
        var newBack : Int = (back+1) % n
        array(back) = x
        println("Das Array sieht nach enqueue wie folgt aus: ")
        val l : List[A] = array.toList
        println(l)
        back = newBack
        resize()                // prüft ob Array vergrößert/verkleinert werden muss

    def dequeue() : A =
        if !isEmpty then
            val result : A = array(front)
            array(front) = null.asInstanceOf[A]
            println("Das Array sieht nach dequeue wie folgt aus: ")
            val l : List[A] = array.toList
            println(l)
            // one position further in the cyclic ordering
            front = (front+1) % n
            resize()                // prüft ob Array vergrößert/verkleinert werden muss
            result
        else throw new Exception("The queue ist empty")
    
    // The queue is empty if and only if front == back.
    def isEmpty : Boolean = front == back
    def size : Int = (n+back-front) % n


def test2b() : Unit =  
    val testQueue : MyQueue[Int] = new DynamicArrayQueue[Int]
    for i <- 0 to 7 do
        testQueue.enqueue(i)
    println("Jetzt beginnt dequeue: ")
    while !testQueue.isEmpty do
        testQueue.dequeue()

// --------------------------------------------------------------------------------------------------------------------------------

/* Aufgabe 2c

Vergleich der drei Implementierungen des ADT Stack:

Beschränkungen der Implementierungen:

Hier kann im Vergleich festgestellt werden, dass die Implementierung mit LinkedNodes und die Implementierung mit dynamischem Array
keine Beschränkungen haben was die Größe der Queue angegeht. Die Implementierung mit einem statischen Array ist hingegen durch
die feste Größe des Arrays beschränkt.

Komplexität der Implementierungen:

Hier kann im Vergleich festgestellt werden, dass die Implementierung mit LinkedNodes und die Implementierung mit statischem Array
einen weniger komplexen Quellcode haben, wodurch das Programm einfacher zu verstehen ist. Im Gegensatz dazu ist die Komplexität
der Implementierung mit einem dynamischen Array größer, da zusätzlich die Funktion resize implementiert werden muss.

Speichereffizienz der Implementierungen:

Hier kann im Vergleich festgestellt werden, dass die Speichereffizienz der Implementierung mit LinkedNodes am besten ist, da die
Queue genau so viele Elemente besitzt wie sie auch insgesamt haben soll. Die nächst beste Speichereffizienz besitzt die
Implementierung mit einem dynamischen Array, da die Größe des Arrays durch die Funktion resize passend skaliert wird, wodurch die 
Anzahl der leeren Elemente des Arrays minimiert wird. Die Implementierung mit einem statischen Array hat dabei die schlechteste
Speichereffizienz, da die Größe des Arrays garnicht erst skaliert, wodurch viele leere Elemente im Arrys auftreten können.

Laufzeit der Implementierungen:

Hier kann im Vergleich festgestellt werden, dass die Implementierung mit einem dynamischen Array und die Implementierung mit 
einem statischen Array besser sind, da sie anders als eine Queue mit LinkedNodes, ein sehr gutes Cachingverhalten haben.

*/

// ----------------------------------------------------------------------------------------------------------------------------------

// Aufgabe 2d

import scala.reflect.ClassTag

// Precondition: None
// Effect: None
// Result: An empty stack of maximum capacity max(1, capacity) is returned.
class ArrayQueue[A : ClassTag](capacity : Int) extends MyQueue[A]:
    // The header:
    private val n : Int = if capacity < 1 then 1 else capacity
    private val array : Array[A] = new Array[A](n)
    private var front : Int = 0
    private var back : Int = 0
    private var isFull : Boolean = false    // zusätzliche Variable um zu prüfen ob die Queue voll ist

    def enqueue(x : A) : Unit = 
        println("Hier enqueue: ")
        if isFull == false then             // wenn Queue nicht voll, dann kann enqueue ausgeführt werden
            array(back) = x
            var l2 : List[A] = array.toList
            println("Das Array sieht nach enqueue wie folgt aus: ")
            println(l2)
            back = (back+1)%n
            if back == front then isFull = true             // Stack ist voll, wenn nach enqueue back == front
        else throw new Exception("The queue is full")

    def dequeue() : A =
        if !isEmpty then
            val result : A = array(front)
            array(front) = null.asInstanceOf[A]
            // one position further in the cyclic ordering
            front = (front + 1) % n
            isFull = false                  // Queue ist nicht mehr voll, wenn ein Element "bearbeitet" wurde
            result
        else throw new Exception("The queue is empty")

    // The queue is empty if front == back ans isFull == false
    def isEmpty : Boolean = if front == back && isFull == false then true else false
    def size : Int = (n+back-front) % n

def test2d() : Unit =  
    val testQueue : MyQueue[Int] = new ArrayQueue[Int](11)
    for i <- 0 to 10 do
        testQueue.enqueue(i)
    for i <- 0 to 10 do
        println(testQueue.dequeue())

// ---------------------------------------------------------------------------------------------------------------------------------

// Aufgabe 2e

