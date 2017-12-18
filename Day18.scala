import scala.annotation.tailrec

trait Operand
case class FixedNumber(value: Int) extends Operand
case class Register(name: Char) extends Operand

trait Command
case class SetCmd(register: Char, value: Operand) extends Command
case class Add(register: Char, value: Operand) extends Command
case class Multiply(register: Char, value: Operand) extends Command
case class Modulo(register: Char, value: Operand) extends Command
case class Send(value: Operand) extends Command
case class Recover(register: Char) extends Command
case class JumpIfPositive(register: Operand, offset: Operand) extends Command

implicit class Regex(sc: StringContext) {
  def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
}

def parseCommands(text: String) = {
  def parseOperand(text: String) = text match 
  {
    case r"[a-z]" => Register(text(0))
    case _ => FixedNumber(text.toInt)
  }
  
  def parseRegister(text: String) = text match 
  {
    case r"[a-z]" => text(0)
  }
  
  def parseCommand(line: String) = line.split("\\s+") match {
    case Array("set", a, x) => SetCmd(parseRegister(a), parseOperand(x))
    case Array("add", a, x) => Add(parseRegister(a), parseOperand(x))
    case Array("mul", a, x) => Multiply(parseRegister(a), parseOperand(x))
    case Array("mod", a, x) => Modulo(parseRegister(a), parseOperand(x))
    case Array("snd", a) => Send(parseOperand(a))
    case Array("rcv", a) => Recover(parseRegister(a))
    case Array("jgz", a, x) => JumpIfPositive(parseOperand(a), parseOperand(x))
  }
  
  text.trim().split("\n").map(_.trim()).map(parseCommand).toList
}

def getOperand(operand: Operand, registers: Map[Char, BigInt]): BigInt = operand match {
  case FixedNumber(v) => v
  case Register(n) => registers(n)
}

def firstFirstRecovered(commands: List[Command]) = {
  
  @tailrec
  def impl(address: Int, currentSent: BigInt, registers: Map[Char, BigInt]): BigInt = {
    val command = commands(address)
    command match { 
      case Recover(r) if registers(r) > 0 => currentSent
      case Send(v) => impl(address + 1, getOperand(v, registers), registers)
      case JumpIfPositive(r, o) if getOperand(r, registers) > 0 => 
        impl(address + getOperand(o, registers).toInt, currentSent, registers)
      case _ => {
        val newRegisters = command match {
          case SetCmd(r, v) => registers + (r -> getOperand(v, registers))
          case Add(r, v) => registers + (r -> (registers(r) + getOperand(v, registers)))
          case Multiply(r, v) => registers + (r -> (registers(r) * getOperand(v, registers)))
          case Modulo(r, v) => registers + (r -> (registers(r) % getOperand(v, registers)))
          case _ => registers
        }
        impl(address + 1, currentSent, newRegisters)
      }
    }
  }
  
  val registers = Map[Char, BigInt]().withDefaultValue(BigInt(0))
  impl(0, -1, registers)
}

case class WorkerState(address: Int, registers: Map[Char, BigInt], inbox: List[BigInt])

def executeDuel(commands: List[Command]) = {

  def step(state: WorkerState): (WorkerState, Option[BigInt]) = {
    val WorkerState(address, registers, inbox) = state
    val command = commands(state.address)
    val newState = command match { 
      case Recover(r) => inbox match {
        case m :: rem => WorkerState(address + 1, registers + (r -> m), rem)
        case List() => state
      }
      case JumpIfPositive(r, o) if getOperand(r, registers) > 0 => 
        WorkerState(address + getOperand(o, registers).toInt, registers, inbox)
      case _ => {
        val newRegisters = command match {
          case SetCmd(r, v) => registers + (r -> getOperand(v, registers))
          case Add(r, v) => registers + (r -> (registers(r) + getOperand(v, registers)))
          case Multiply(r, v) => registers + (r -> (registers(r) * getOperand(v, registers)))
          case Modulo(r, v) => registers + (r -> (registers(r) % getOperand(v, registers)))
          case _ => registers
        }
        WorkerState(address + 1, newRegisters, inbox)
      }
    }
    val outbox =  command match {
      case Send(v) => Some(getOperand(v, registers))
      case _ => None
    }

    (newState, outbox)
  }

  @tailrec
  def impl(state0: WorkerState, state1: WorkerState, acc: Int): Int = {
    def mergeQueue(state: WorkerState, outbox: Option[BigInt]) = outbox match {
      case Some(m) => WorkerState(state.address, state.registers, state.inbox ::: List(m))
      case None => state
    }

    val (interimState0, outbox0) = step(state0)
    val interimState1 = mergeQueue (state1, outbox0)
    
    val (newState1, outbox1) = step(interimState1)
    val newState0 = mergeQueue(interimState0, outbox1)
    
    val newAcc = outbox1 match {
      case Some(_) => acc + 1
      case None => acc
    }
    
    if (newState0 == state0 && newState1 == state1) newAcc
    else impl(newState0, newState1, newAcc)
  }
  
  val registers0 = Map[Char, BigInt]('p' -> 0).withDefaultValue(BigInt(0))
  val registers1 = Map[Char, BigInt]('p' -> 1).withDefaultValue(BigInt(0))
  impl(
    WorkerState(0, registers0, List()), 
    WorkerState(0, registers1, List()),
    0)
}


val input = """
snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d
""" // Your input

val commands = parseCommands(input)
println("d18p1> " + firstFirstRecovered(commands))
println("d18p2> " + executeDuel(commands))