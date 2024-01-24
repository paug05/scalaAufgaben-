// Übungsblatt 11
// Paul G. und Tim D.
// 16.01.2024


/* Aufgabe 1a

Scala verwendet die Referenzsemantik bei Zuweisungen.

Beispiel:

var a : List[Int] = List(1,2,3,4,5)
var b : List[Int] = a

Die Variablen a und b haben in diesem Fall die selbe Id, da durch die Referenzsemantik ein Alias
erstellt wird anstatt, dass die Liste von a für b kopiert wird. 

Veranschaulichung: (Bei Veränderung einer der Variablen wird auch die andere verändert.)

b(4) = 6

==> var b : List[Int] = List(1,2,3,4,6)
    var a : List[Int] = List(1,2,3,4,6)


Parameterübergabe-Konventionen in Scala:

1. Call by Value: Der Wert eines eingegebenen Ausdrucks wird zuerst berechnet und dann der
                  Funkton übergeben.

2. Call by Name: Eingegebene Ausdrücke werden erst in der Funktion ausgewertet.

Call by Value wird standard gemäß in Scala genutzt, aber man kann auch mit "=>" die Call by Name
Konvention benutzen. 

*/



// Aufgabe 1b

// siehe PDF-Dokument

/*
Der Ausdruck "a(1) == c(1)" wertet zu true aus, da das Array an Stelle a(1) dasselbe ist, wie das
an  der Stelle c(1).
Der Ausdruck "b(2) == c(2)" wertet zu false aus, da c(2) auf a(1) zeigt und b und a das selbe Objekt
sind, wodurch b(2) niemals das gleiche wie c(2) sein kann.
Der Ausdruck "a == c" wertet zu false aus, da a und c zwar die selben Elemente besitzen jedoch durch
den "a.clone" befehl nicht das selbe Objekt sind.
Der Ausdruck "b(2)(2)" wertet zu 7 aus, da das Array von b und das Array von a an Stelle 2 an 
der Stelle 2 das Element 7 besitzt.
Der Ausdruck "c(1)(1)" wertet zu 6 aus, da das Array von c an Stelle 1 an der Stelle 1 das Element
6 besitzt.
Der Ausdruck "c(2)(2)" wertet zu 3 aus, da das Array von c an Stelle 2 und das Array von a an
Stelle 1 beide an der Stelle 2 das Element 3 besitzen.
*/


// Aufgabe 1c

class Geom3D () :

    // Vorraussetzung: keine
    // Effekt: keiner
    // Ergebnis: Das Volumen des Körpers ist geliefert.
    def volume() : Double = 0.0

    // Vorraussetzung: keine
    // Effekt: keiner
    // Ergebnis: Die Oberfläche des Körpers ist geliefert.
    def surfaceArea() : Double = 0.0

class Wuerfel (private var a : Double) extends Geom3D () :

    override def volume() : Double =
        this.a * this.a * this.a
    
    override def surfaceArea() : Double =
        (this.a * this.a) * 6

class Quader (private var a : Double, private var b : Double, private var h : Double) extends Geom3D () :

    override def volume() : Double =
        this.a * this.b * this.h
    
    override def surfaceArea() : Double =
        (this.h * this.b) * 4 + (this.a * this.h) * 2

class Kugel (private var r : Double) extends Geom3D () :

    override def volume() : Double =
        this.r * this.r * this.r * 4/3 * math.Pi
    
    override def surfaceArea() : Double =
        4 * (this.r * this.r) * math.Pi

class Tetraeder (private var a : Double) extends Geom3D () :

    override def volume() : Double =
        math.sqrt(2) * (this.a * this.a * this.a)/12

    override def surfaceArea() : Double =
        (this.a * this.a) * math.sqrt(3)

// Vorraussetzung: Die Elemente des Array list haben den Typ Geom3D.
// Effekt: Das Volumen des jeweils betrachteten Elements des Arrays wird auf Gesamtvolumen addiert. Die Oberfläche des jeweils
//         betrachteten Elements des Arrays wird auf die Gesamtoberfläche addiert.
// Ergebnis: Die Gesamtoberfläche und das Gesamtvolumen aller Elemente des Arrays ist geliefert.

def gesamt(list : Array[Geom3D]) : (Double,Double) =
    var gesamtVol : Double = 0.0     
    var gesamtSur : Double = 0.0         
    for x <- list do                                // alle Elemente des Arrays list werden nacheinander durchlaufen
        gesamtVol = gesamtVol + x.volume()          // das Volumen von x wird mit dem Gesamtvolumen addiert
        gesamtSur = gesamtSur + x.surfaceArea()     // die Oberfläche von x wird mit der Gesamtoberfläche addiert
    return (gesamtVol,gesamtSur)

// Tests für die Funktion gesamt:

@main 
def geom3Demo() : Unit =
    println(gesamt(Array[Geom3D](Wuerfel(12),Kugel(5),Quader(5,4,6))) == (2371.5987755982987,1334.1592653589794))
    println(gesamt(Array[Geom3D](Wuerfel(1),Wuerfel(1))) == (2.0,12.0))
    println(gesamt(Array[Geom3D](Wuerfel(12),Kugel(5),Tetraeder(4),Wuerfel(60),Quader(5,4,6))) == (218379.14124793097,22961.87207828008))
    println(gesamt(Array[Geom3D](Wuerfel(3),Quader(2,3,4),Kugel(1.5),Tetraeder(10))) == (182.988297138912,319.47941463919585))



/* Aufgabe 1d

Die Ausgabe bei einem Aufruf der main-Funktion test ist:

    >>> Read it.
    >>> Ship it.
    >>> Buy it.
    >>> Read it.
    >>> Box it.

Dies lässt sich durch die Scala-Methode asInstanceOf erklären, da diese bewirkt, dass die
Variable obj, die eigentlich ein Objekt der Klasse Any ist, sich selbst als ein Objekt einer 
anderen Klasse "sieht", wodurch sie die entsprechend definierte Methode für die jeweilige 
Klasse ausführt. 

*/

// Aufgabe 2a

// Vorraussetzung: keine
// Effekt: Für jeden Durchlauf der while-Schleife wird die Rundenanzahl um eins erhöht.
// Ergebnis: Wenn die Zahl erraten wurde endet das Spiel und die benötigte Anzahl an Runden ist geliefert.

def zahlenRaten() : Unit =
    import scala.util.Random
    import scala.io.StdIn.readLine

    var runden : Int = 0
    val z : Int = Random.between(1,100)

    println("Bitte gib deine Zahl ein: ")
    var tipp : Int = readLine().toInt

    while z != tipp do
        if z > tipp then
            println("Die Zahl ist größer als dein Tipp.")
            println("Bitte gib deine Zahl ein: ")
            runden = runden + 1
            tipp = readLine().toInt
        else
            println("Die Zahl ist kleiner als dein Tipp.")
            println("Bitte gib deine Zahl ein: ")
            runden = runden + 1
            tipp = readLine().toInt

    println("Wohooooooooo! Gewonnen :)")
    println("Du hast " + runden.toString + " Runden gebraucht.")

// Aufgabe 2b/2c

class Fahrzeug(private var baujahr: Int, private var reifen: Int, private var wert: Double, private var hersteller: String) :

    private  val nummernschild = Fahrzeug.nummernschild_counter
    Fahrzeug.nummernschild_counter = Fahrzeug.nummernschild_counter + 1

    //Get-Methoden

    def getBaujahr: Int = this.baujahr

    def getReifen: Int = this.reifen

    def getWert: Double = this.wert

    def getHersteller: String = this.hersteller

    def getNummernschild: Int = this.nummernschild

    // speziellere Funktionen


    // Voraussetzung: keine
    // Effekt: kein Effekt
    // Ergebnis: die Art des Fahrzeugs ist anhand der Reifenanzahl gegeben

    def art(): String = 
        if this.reifen == 2 then 
            "Es handelt sich um ein Motorrad."
        else if this.reifen <= 4 then 
            "Es handelt sich um ein Auto."
        else if this.reifen == 6 then 
            "Es handelt sich um ein LKW."
        else 
            "So ein Fahzeug ist uns nicht bekannt."


    // Voraussetzung: keine
    // Effekt: keine 
    // Ergebnis: der Wert des Fahrzeugs ist durch den Grad des Unfalls gesunken

    def unfall(grad: Int): Unit =
        if grad ==1 then this.wert = this.wert -50
        else if grad <=5 && grad >= 2 then this.wert = this.wert -200
        else this.wert = 0

    // Voraussetzung; keine
    // Effekt: keine
    // Ergebnis: Es ist gegeben ob das Fahrzeug ein Oldtimer ist (älter als 30 Jahre) oder nicht.

    def istOldTimer() : String =
        if (2024 - this.baujahr) >= 30 then
            "Du besitzt einen OldTimer."
        else
            "Du besitzt in " + (30-(2024-this.baujahr)).toString + " Jahren einen OldTimer."



    //Voraussetzung: keine
    // Effekt: keine
    // Ergebnis: das Lieblings Fahrzeug ist gegeben

    def lieblings(): Unit = println("Ich habe kein Lieblings Fahrzeug")
        

object Fahrzeug:
    private var nummernschild_counter: Int = 4000

    //Voraussetzung: keine
    // Effekt: keine
    // Ergebnis: Der Nummernschildzähler ist auf die gewünschte Zahl gestellt.

    def resetNummernschild(neues_schild: Int): Unit =
        nummernschild_counter = neues_schild

// Exemplare von der Klasse Fahrzeug zur verdeutlichung der Referenzsemantik:

val f = Fahrzeug(1990,4,23000.0,"Mercedes")
val g = f
val h = g
val b = Fahrzeug(2005,2,12000,"BMW")

g.unfall(2)         // Durch die Referenzsemantik ändert sich der Wert von h und f genauso wie der von g.
g.getWert
h.getWert
f.getWert
b.getWert

b.unfall(2)         // Dadurch, dass b ein anderes Objekt als f, g und h ist, haben Änderung mit b keine Auswirkungen auf f, g oder h.
b.getWert
f.getWert
h.getWert

/* 
Dadurch, dass f, g und h das selbe Objekt sind, wobei g und h ein Alias für f sind durch die Referenzsemantik, wirken sich
Änderung mit einer von diesen Values auf alle anderen Values gleichermaßen aus. Nur auf das Objekt b haben Änderungen mit den
Values f, g und h keine Auswirkung, da dies ein anderes Objekt ist.
*/

    

// Aufgabe 2d

class Motorrad(baujahr: Int, reifen :Int, wert: Double, hersteller: String) extends Fahrzeug(baujahr, reifen, wert, hersteller) :


    private var fußrasten: Boolean = false 

    def sindRastenraus(): Boolean = this.fußrasten

    def rasten(): Unit = this.fußrasten = true 

    override  def lieblings(): Unit = println("Motorräder sind meine Lieblings-Fahrzeuge!")



class LKW(baujahr: Int, reifen :Int, wert: Double, hersteller: String) extends Fahrzeug(baujahr, reifen, wert, hersteller) :

    private var ladung : Int = 0

    def neueLadung(gewicht: Int): Unit = this.ladung = gewicht

    def mehrLadung(gewicht :Int): Unit = this.ladung = this.ladung + gewicht

    override  def lieblings(): Unit = println("LKWs sind meine Lieblings-Fahrzeuge!")



class Fahrrad(baujahr: Int, reifen :Int, wert: Double, hersteller: String) extends Fahrzeug(baujahr, reifen, wert, hersteller) :

    private var helm: Boolean = false 

    def helmAufsetzen():Unit = this.helm = true

    def helmAbsetzen(): Unit = this.helm = false

    def istHelmAufgesetzt(): Boolean = this.helm

    override  def lieblings(): Unit = println("Ich fahre am liebsten mit dem Fahrrad!")

/*

Inklusionspolymorphie:

Einer Variable vom Typ A, kann ein Objekt vom Typ B zugeordnet werden, falls B eine
Unterklasse von A ist.


Beispiel zur Verdeutlichung:

var f : Fahrzeug = Fahrrad(2015,2,200,"Stadler")
f = LKW(2005,6,50000,"MAN")

Obwohl f den Datentyp Fahrzeug hat kann es zu einem Objekt der Klasse Fahrrad zugewiesen werden. Außerdem kann f ein neues Objekt
von einer nochmal anderen Klasse zugewiesen werden. Dies wird durch die Inklusionspolymorphie möglich.

Die Variable f hat den statischen Datentyp Fahrzeug, wodurch nur Funktionen des statischen Datentyps für f verfügbar sind. f hat
außerdem den dynamischen Datentypen LKW, da dies der Typ der letzten Zuweisung für f war.

*/