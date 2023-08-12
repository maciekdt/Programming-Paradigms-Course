//Maciej Dutkowski
import scala.runtime.Nothing$


//Zadanie 1
class Time_1(private var privHour: Int):
  if (privHour < 0) privHour = 0

  def hour: Int = privHour

  def hour_=(newHour: Int) =
    if(newHour < 0) privHour = 0
    else privHour = newHour



//Test zadanie 1
object Test_1:
  def main(args: Array[String]): Unit =
    println("Test zad1 results : ")
    val time1 = new Time_1(10)
    val time2 = new Time_1(-5)
    val time3 = new Time_1(3)

    time1.hour = 2
    time3.hour = -6

    println(time1.hour == 2)
    println(time2.hour == 0)
    println(time3.hour == 0)







//Zadanie 2a
class Time_2a(private var privHour:Int, private var privMinute:Int):
  if(privHour < 0 || privHour > 23) throw new IllegalArgumentException
  if(privMinute < 0 || privMinute > 59) throw new IllegalArgumentException

  def hour: Int = privHour

  def hour_=(newHour:Int) =
    if(newHour < 0 || newHour > 23) throw new IllegalArgumentException
    else privHour = newHour

  def minute: Int = privMinute

  def minute_=(newMinute:Int) =
    if(newMinute < 0 || newMinute > 59) throw new IllegalArgumentException
    else privMinute = newMinute

  def before(other:Time_2a):Boolean =
    if(privHour > other.privHour) return true
    else if(privHour == other.privHour  &&  privMinute > other.privMinute) return true
    else return false


//Test zadanie 2a
object Test_2a:
  def main(args: Array[String]): Unit =
    println("Test zad2a results : ")
    val time1 = new Time_2a(17, 33)
    val time2 = new Time_2a(14, 2)

    println(time1.before(time2))
    println(!time2.before(time1))

    time1.hour = 14
    println(time1.before(time2))

    try{
      val time3 = new Time_2a(27, 33)
      println("false")
    }
    catch {
      case _ : IllegalArgumentException => println("true")
      case _ => println("false")
    }

    try{
      val time3 = new Time_2a(22, 33)
      time3.minute = 100
      println("false")
    }
    catch {
      case _ : IllegalArgumentException => println("true")
      case _ => println("false")
    }








//Zadanie 2b
class Time_2b(initHour:Int, initMinute:Int):
  if(initHour < 0 || initHour > 23) throw new IllegalArgumentException
  if(initMinute < 0 || initMinute > 59) throw new IllegalArgumentException
  private var minutesAfterMid:Int = initHour*60 + initMinute

  def hour: Int = minutesAfterMid/60

  def hour_=(newHour:Int) =
    if(newHour < 0 || newHour > 23) throw new IllegalArgumentException
    else minutesAfterMid = newHour*60 + minute

  def minute: Int = minutesAfterMid%60

  def minute_=(newMinute:Int) =
    if(newMinute < 0 || newMinute > 59) throw new IllegalArgumentException
    else minutesAfterMid = hour*60 + newMinute

  def before(other:Time_2b):Boolean =
    if(minutesAfterMid > other.minutesAfterMid) return true
    else return false


//Test zad 2b
object Test_2b:
  def main(args: Array[String]): Unit =
    println("Test zad2a results : ")
    val time1 = new Time_2b(17, 33)
    val time2 = new Time_2b(14, 2)

    println(time1.before(time2))
    println(!time2.before(time1))

    time1.hour = 14
    println(time1.before(time2))

    time1.hour = 4
    time1.minute = 54
    println(time1.hour == 4  &&  time1.minute == 54)

    try{
      val time3 = new Time_2a(27, 33)
      println("false")
    }
    catch {
      case _ : IllegalArgumentException => println("true")
      case _ => println("false")
    }

    try{
      val time3 = new Time_2a(22, 33)
      time3.minute = 100
      println("false")
    }
    catch {
      case _ : IllegalArgumentException => println("true")
      case _ => println("false")
    }







//Zadanie 3
class Pojazd(val producent:String, val model:String, val rokProdukcji:Int = -1, var numerRejestracyjny:String =""){
  override def toString: String = producent + " " + model + " " + rokProdukcji + " " + numerRejestracyjny
}


//Test zad 3
object Test_3:
  def main(args: Array[String]): Unit =
    val pojazd1 = new Pojazd("Pagani", "Zonda", 1992, "SI6745")
    val pojazd2 = new Pojazd("Ferrari", "328", 2011)
    val pojazd3 = new Pojazd("Mazda", "CX5")
    val pojazd4 = new Pojazd("Fiat", "Punto", numerRejestracyjny = "DJ8756")

    println(pojazd1)
    println(pojazd2)
    println(pojazd3)
    println(pojazd4)





//Zadanie 4
object UzycieWyjatkow:
  def main(args: Array[String]): Unit =
    try{
      metoda1()
    }
    catch{
      case e:Throwable => {
        System.err.println(e.getMessage + "\n")
        e.printStackTrace()
      }
    }

  def metoda1():Unit =
    metoda2()

  def metoda2():Unit =
    metoda3()

  def metoda3():Unit =
    throw new Exception("Wyjatek zgloszony w metoda3")





















