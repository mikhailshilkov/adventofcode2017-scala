import scala.annotation.tailrec

implicit class Regex(sc: StringContext) {
  def r = new util.matching.Regex(sc.parts.mkString, sc.parts.tail.map(_ => "x"): _*)
}

trait Operand
case class FixedNumber(value: Int) extends Operand
case class Register   (name: Char) extends Operand

trait Command
case class SetCmd        (register: Char, value: Operand)  extends Command
case class Substract     (register: Char, value: Operand)  extends Command
case class Multiply      (register: Char, value: Operand)  extends Command
case class JumpIfNotZero (value: Operand, offset: Operand) extends Command

def parseCommands(text: String) = {
  def parseOperand(text: String) = text match 
  {
    case r"[a-h]" => Register(text(0))
    case _ => FixedNumber(text.toInt)
  }
  
  def parseRegister(text: String) = text match { case r"[a-h]" => text(0) }
  
  def parseCommand(line: String) = line.split("\\s+") match {
    case Array("set", a, x) => SetCmd(parseRegister(a), parseOperand(x))
    case Array("sub", a, x) => Substract(parseRegister(a), parseOperand(x))
    case Array("mul", a, x) => Multiply(parseRegister(a), parseOperand(x))
    case Array("jnz", a, x) => JumpIfNotZero(parseOperand(a), parseOperand(x))
  }
  
  text.trim().split("\n").map(_.trim()).map(parseCommand).toList
}

implicit class Registers(val value: Map[Char, BigInt]) extends AnyVal {
  def getOperand(operand: Operand): BigInt =  operand match {
    case FixedNumber(v) => v
    case Register(n) => value(n)
  }
  
  def apply(command: Command) = command match {
    case SetCmd(r, v)    => value + (r -> value.getOperand(v))
    case Substract(r, v) => value + (r -> (value(r) - value.getOperand(v)))
    case Multiply(r, v)  => value + (r -> (value(r) * value.getOperand(v)))
    case _ => value
  }
}

def countMultiplications(commands: List[Command]) = {
  
  @tailrec
  def impl(address: Int, acc: BigInt, registers: Map[Char, BigInt]): BigInt = {
    if (address >= commands.length) acc 
    else {
      val command = commands(address)
      command match { 
        case JumpIfNotZero(r, o) if registers.getOperand(r) != 0 => 
          impl(address + registers.getOperand(o).toInt, acc, registers)
        case _ => {
          val newRegisters = registers.apply(command)
          val newAcc = command match { case Multiply(_, _) => acc + 1 case _ => acc }
          impl(address + 1, newAcc, newRegisters)
        }
      }
    }
  }
  
  val registers = Map[Char, BigInt]().withDefaultValue(BigInt(0))
  impl(0, 0, registers)
}

val input = """
set b 79
set c b
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23
""" // Your input (only works for part 1)

val commands = parseCommands(input)
println("d23p1> " + countMultiplications(commands))

// Part 2 is desassembled code of my own input; won't work for others

def isComposite(b: Int) = 
  (2 to math.sqrt(b).toInt) exists (d => b % d == 0)
val h = (107900 to 124900 by 17).count(isComposite)
println("d23p2> " + h)