class Buffer(var patientsNumber: Integer):
  private var empty = true
  private var handled = false;

  def enter() = this.synchronized {
    while !empty do wait()
    empty = false
    println(Thread.currentThread.getName() + " entered the room")
    notifyAll()
  }

  def leave() = this.synchronized {
    while !handled do wait()
    empty = true
    handled = false
    println(Thread.currentThread.getName() + " left the room")
    notifyAll()
  }

  def handle() = this.synchronized{
    while (empty || handled) do wait()
    handled = true
    println(Thread.currentThread.getName() + " handled the patient")
    notifyAll()
  }

end Buffer


class Patient(name: String, buf: Buffer) extends Thread(name) :
  override def run: Unit =
      buf.enter()
      buf.leave()
end Patient


class Doctor(name: String, buf: Buffer) extends Thread(name) :
  override def run: Unit =
    for i <- 1 to buf.patientsNumber do
      buf.handle()
end Doctor


object Simulation:
  def main(args: Array[String]): Unit =
    val buf = new Buffer(3)
    new Patient("Patient 1", buf).start()
    new Patient("Patient 2", buf).start()
    new Patient("Patient 3", buf).start()
    new Doctor("Doctor 1", buf).start()
end Simulation
