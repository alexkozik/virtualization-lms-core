package scala.virtualization.lms.internal

trait AbstractHostTransfer {

  val IR: Expressions
  import IR._

  def emitSend(sym: Sym[Any], host: Hosts.Value): (String,String)
  def emitRecv(sym: Sym[Any], host: Hosts.Value): (String,String)
  def emitSendView(sym: Sym[Any], host: Hosts.Value): (String,String)
  def emitRecvView(sym: Sym[Any], host: Hosts.Value): (String,String)
  def emitSendUpdate(sym: Sym[Any], host: Hosts.Value): (String,String)
  def emitRecvUpdate(sym: Sym[Any], host: Hosts.Value): (String,String)
}

trait AbstractDeviceTransfer {

  val IR: Expressions
  import IR._

  def emitSendSlave(sym: Sym[Any]) : (String,String)
  def emitRecvSlave(sym: Sym[Any]) : (String,String)
  //def emitSendViewSlave(sym: Sym[Any]) : (String,String)
  //def emitRecvViewSlave(sym: Sym[Any]) : (String,String)
  def emitSendUpdateSlave(sym: Sym[Any]) : (String,String)
  def emitRecvUpdateSlave(sym: Sym[Any]) : (String,String)

  //def allocOutput(newSym: Sym[_], sym: Sym[_], reset: Boolean = false) : Unit
}

object Hosts extends Enumeration {

  val JVM = Value("jvm")
  val CPP = Value("cpp")

  def host(s: String): Value = s.toLowerCase() match {
    case "jvm" => JVM
    case "cpp" => CPP
    case _ => throw new IllegalArgumentException("unsupported host: " + s)
  }
}