import scala.annotation.tailrec

implicit class Regex(sc: StringContext) {
  def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
}

trait Operand
case class FixedNumber(value: Int) extends Operand
case class Register   (name: Char) extends Operand

trait Command
case class SetCmd        (register: Char, value: Operand)  extends Command
case class Add           (register: Char, value: Operand)  extends Command
case class Multiply      (register: Char, value: Operand)  extends Command
case class Modulo        (register: Char, value: Operand)  extends Command
case class Send          (value: Operand)                  extends Command
case class Recieve       (register: Char)                  extends Command
case class JumpIfPositive(value: Operand, offset: Operand) extends Command

def parseCommands(text: String) = {
  def parseOperand(text: String) = text match 
  {
    case r"[a-z]" => Register(text(0))
    case _ => FixedNumber(text.toInt)
  }
  
  def parseRegister(text: String) = text match { case r"[a-z]" => text(0) }
  
  def parseCommand(line: String) = line.split("\\s+") match {
    case Array("set", a, x) => SetCmd(parseRegister(a), parseOperand(x))
    case Array("add", a, x) => Add(parseRegister(a), parseOperand(x))
    case Array("mul", a, x) => Multiply(parseRegister(a), parseOperand(x))
    case Array("mod", a, x) => Modulo(parseRegister(a), parseOperand(x))
    case Array("snd", a)    => Send(parseOperand(a))
    case Array("rcv", a)    => Recieve(parseRegister(a))
    case Array("jgz", a, x) => JumpIfPositive(parseOperand(a), parseOperand(x))
  }
  
  text.trim().split("\n").map(_.trim()).map(parseCommand).toList
}

implicit class Registers(val value: Map[Char, BigInt]) extends AnyVal {
  def getOperand(operand: Operand): BigInt =  operand match {
    case FixedNumber(v) => v
    case Register(n) => value(n)
  }
  
  def apply(command: Command) = command match {
    case SetCmd(r, v)   => value + (r -> value.getOperand(v))
    case Add(r, v)      => value + (r -> (value(r) + value.getOperand(v)))
    case Multiply(r, v) => value + (r -> (value(r) * value.getOperand(v)))
    case Modulo(r, v)   => value + (r -> (value(r) % value.getOperand(v)))
    case _ => value
  }
}

def firstFirstRecovered(commands: List[Command]) = {
  
  @tailrec
  def impl(address: Int, currentSent: BigInt, registers: Map[Char, BigInt]): BigInt = {
    val command = commands(address)
    command match { 
      case Recieve(r) if registers(r) > 0 => currentSent
      case Send(v) => impl(address + 1, registers.getOperand(v), registers)
      case JumpIfPositive(r, o) if registers.getOperand(r) > 0 => 
        impl(address + registers.getOperand(o).toInt, currentSent, registers)
      case _ => {
        val newRegisters = registers.apply(command)
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
    val command = commands(address)
    val newState = command match { 
      case Recieve(r) => inbox match {
        case m :: rem => WorkerState(address + 1, registers + (r -> m), rem)
        case List() => state
      }
      case JumpIfPositive(r, o) if registers.getOperand(r) > 0 => 
        WorkerState(address + registers.getOperand(o).toInt, registers, inbox)
      case _ => {
        val newRegisters = registers.apply(command)
        WorkerState(address + 1, newRegisters, inbox)
      }
    }
    val outbox =  command match {
      case Send(v) => Some(registers.getOperand(v))
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
    val interimState1 = mergeQueue(state1, outbox0)
    
    val (newState1, outbox1) = step(interimState1)
    val newState0 = mergeQueue(interimState0, outbox1)
    
    val newAcc = acc + (outbox1 match { case Some(_) => 1 case None => 0 })
    
    if (newState0 == state0 && newState1 == state1) newAcc
    else impl(newState0, newState1, newAcc)
  }
  
  val registers0 = Map[Char, BigInt]('p' -> 0).withDefaultValue(BigInt(0))
  val registers1 = Map[Char, BigInt]('p' -> 1).withDefaultValue(BigInt(0))
  impl(WorkerState(0, registers0, List()), WorkerState(0, registers1, List()), 0)
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