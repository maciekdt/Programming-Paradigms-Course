

class Zwierzak(val gatunek:String, val imie:String, val rokUrodzenia:Integer) :
  override def toString() : String = "Zwierze :  " + imie + " (" + gatunek + ") " + rokUrodzenia.toString()



class Obora(val wlasciciel:String, val liczbaBoksow:Int) :
  val numerObory = java.util.UUID.randomUUID.toString
  var zwierzaki = List[Zwierzak]()

  def kwateruj(nowyZwierzak:Zwierzak) =
    if(zwierzaki.length < liczbaBoksow) zwierzaki = nowyZwierzak :: zwierzaki
    else throw new Exception

  def wykwateruj(zwierzak:Zwierzak) =
    def wykwateruj_(pozostaleZwierzaki:List[Zwierzak]): List[Zwierzak] =
      pozostaleZwierzaki match
        case h::l =>
          if(h == zwierzak) wykwateruj_(l)
          else h::wykwateruj_(l)
        case Nil => Nil
    zwierzaki = wykwateruj_(zwierzaki)

  def przenies(zwierzak:Zwierzak, innaObora:Obora) =
    innaObora.wykwateruj(zwierzak)
    kwateruj(zwierzak)

  def wypiszObore() =
    println(this)
    def wypiszObore_(pozostaleZwierzaki:List[Zwierzak]): Unit =
      pozostaleZwierzaki match
        case h::l => {
          println(h)
          wypiszObore_(l)
        }
        case Nil => println("\n")
    wypiszObore_(zwierzaki)
  override def toString() : String = "Obora : " + numerObory + " (" + wlasciciel + ")"







object Test_1 :
  def main(args: Array[String]): Unit =
    val z1 = new Zwierzak("Ryba", "Rekin", 1234)
    val z2 = new Zwierzak("Malpa", "Goryl", 234)
    val z3 = new Zwierzak("Owad", "Osa", 1234)

    val o1 = new Obora("Marek", 123)
    o1.kwateruj(z1)
    o1.kwateruj(z2)
    o1.kwateruj(z3)

    val o2 = new Obora("Ola", 33)
    o2.przenies(z1, o1)

    o1.wypiszObore()
    o2.wypiszObore()

