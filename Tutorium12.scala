// Tutorium 12


// Aufgabe 1 swap Methode für Stack und Queue

trait MyStack[A]:
    // Objects of an arbitrary but fixed type A are stored in a stack.

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
    // Effect: None
    // Result: true is returned if and only if the stack has no elements.
    def isEmpty : Boolean

    // Precondition: None
    // Effect: None
    // Result: The number of elements in the stack is returned.
    def size : Int


trait Swapable[A]:

    // Voraussetzung: der Stack hat mindestens 2 Elemente
    // Ergebnis: kein
    // Effekt: die ersten beiden Elemente des Stacks sind getauscht

    def swap(): Unit

class LinkedNodesStack[A] extends MyStack[A],Swapable[A]: 

	// ATTENTION: Here, we are using the reference semantics
	// of Scala. The property next does not store the object
	// itself, but a reference to the object. This makes it possible
	// that a class can have a property of its own type. In other
	// programming languages (e.g., C, C++, Pascal), we must
	// explicitly declare this property as a reference, since
	// otherwise we would get a compile time error.
	private class Node(val item: A, val next: Node)

	// Header:
	private var topNode : Node = null
	private var _size : Int = 0

	def push(item: A) : Unit =
		topNode = Node(item, topNode)
		_size = _size + 1

    def swap(): Unit =
        if _size >= 2 then 
            val topItem: A = topNode.item
            topNode.item = topNode.next.item
            topNode.next.item = topItem
        else throw Exception("Stack has less than two elements!")

	def pop() : A =
		if !isEmpty then
			val result: A = topNode.item
			// If the top node was simultaneously the last node
			// in the chain, topNode becomes null:
			topNode = topNode.next
			_size = _size - 1
			result
		else throw Exception("Stack is empty")

	def top : A =
		if !isEmpty then topNode.item
		else throw Exception("Stack is empty")

	// If the stack is empty, than its size is 0 and
	// the topNode is just null.
	def isEmpty : Boolean = topNode == null
	def size : Int = _size 


// @main 
// def test(): Unit = 
//     // Note: testStack is declared of type MyStack, not
//     // LinkedNodesStack. This means that we use the trait to
//     // declare the variable, and not the concrete implementation.
//     // This is a good programming practice, because this
//     // forces us to rely only on the operations of the ADT
//     // Stack, and to disregard the special properties of the
//     // implementation as an LinkedNodesStack.
//     // This makes it easier to extend and replace parts of the
//     // program and decreases the amount of dependencies between
//     // the concrete parts.
// 	val testStack: MyStack[Int] = LinkedNodesStack[Int]()
// 	for i <-  0 to 10 do
// 		testStack.push(i)
// 	while !testStack.isEmpty do
// 		println(testStack.pop())
// 	for i <- 0 to 10 do
// 		testStack.push(i)
// 	while !testStack.isEmpty do
// 		println(testStack.pop())
