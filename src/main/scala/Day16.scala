
import scala.io.Source

object Day16 extends App {

  sealed trait InstructionInputType
  case object RegisterReference extends InstructionInputType
  case object ImmediateValue extends InstructionInputType

  case class InstructionArgs(opcode: Int, inputA: Int, inputB: Int, output: Int)
  case class InstructionDef(name: Symbol, inputA: InstructionInputType, inputB: InstructionInputType, fun: (Int, Int) => Int)

  case class RegisterValues(values: Vector[Int] = Vector.fill(4)(0)) {
    def get(reference: Int): Int = {
      assert(reference >= 0 && reference <= values.size)
      values(reference)
    }

    def instruct(instructionDef: InstructionDef, args: InstructionArgs): RegisterValues = {
      def getInput(instructionInputType: InstructionInputType, input: Int): Int = {
        instructionInputType match {
          case RegisterReference => values(input)
          case ImmediateValue => input
        }
      }
      val output = instructionDef.fun(getInput(instructionDef.inputA, args.inputA), getInput(instructionDef.inputB, args.inputB))
      copy(values =  values.updated(args.output, output))
    }
  }
  case object Sample {
    def parse(lines: Seq[String]): Sample = {
      assert(lines.size == 3)

      def parseValues(line: String, seperatorChar: Char): Vector[Int] =
        line.filter(char => char.isDigit || char == seperatorChar).split(seperatorChar).map(_.toInt).toVector

      val Seq(beforeLine, instructionLine, afterLine) = lines
      val before = RegisterValues(parseValues(beforeLine, seperatorChar = ','))
      val Vector(opcode, a,b, output) = parseValues(instructionLine, ' ')
      val instructionSample = InstructionArgs(opcode, a,b, output)
      val after = RegisterValues(parseValues(afterLine, seperatorChar = ','))
      Sample(before, instructionSample, after)
    }
  }
  case class Sample(before: RegisterValues, args: InstructionArgs, after: RegisterValues)


  def matchingInstructions(sample: Sample): Seq[RegisterValues] = instructions.map { instructionDef =>
      sample.before.instruct(instructionDef, sample.args)
    }.filter(_== sample.after)

  def toBit(boolean: Boolean): Int = if (boolean) 1 else 0
  val instructions: List[InstructionDef]  = List(
    InstructionDef('addr, RegisterReference, RegisterReference, _ + _),
    InstructionDef('addi, RegisterReference, ImmediateValue, _ + _),
    InstructionDef('mulr, RegisterReference, RegisterReference, _ * _),
    InstructionDef('muli, RegisterReference, ImmediateValue, _ * _),
    InstructionDef('banr, RegisterReference, RegisterReference, _ & _),
    InstructionDef('bani, RegisterReference, ImmediateValue, _ & _),
    InstructionDef('borr, RegisterReference, RegisterReference, _ | _),
    InstructionDef('bori, RegisterReference, ImmediateValue, _ | _),
    InstructionDef('setr, RegisterReference, ImmediateValue, (a, _) => a),
    InstructionDef('seti, ImmediateValue, ImmediateValue, (a, _) => a),
    InstructionDef('gtir, ImmediateValue, RegisterReference, (a, b) => toBit(a > b)),
    InstructionDef('gtri, RegisterReference, ImmediateValue, (a, b) => toBit(a > b)),
    InstructionDef('gtrr, RegisterReference, RegisterReference, (a, b) => toBit(a > b)),
    InstructionDef('eqir, ImmediateValue, RegisterReference, (a, b) => toBit(a == b)),
    InstructionDef('eqri, RegisterReference, ImmediateValue, (a, b) => toBit(a == b)),
    InstructionDef('eqrr, RegisterReference, RegisterReference, (a, b) => toBit(a == b)),
  )


  val input1 = Source.fromResource("Day16-1-Input.txt").getLines().filterNot(_.isEmpty).grouped(3).map(Sample.parse).toList
  def part1(samples: List[Sample]): Int = input1.map(matchingInstructions).count(_.size >= 3)

  println(part1(input1))

}
