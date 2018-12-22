
import scala.io.Source

object Day16 extends App {

  sealed trait InstructionInputType
  case object RegisterReference extends InstructionInputType
  case object ImmediateValue extends InstructionInputType

  case class InstructionArgs(opcode: Int, inputA: Int, inputB: Int, output: Int)
  case class InstructionDef(name: Symbol, inputA: InstructionInputType, inputB: InstructionInputType, fun: (Int, Int) => Int)

  case object RegisterValues {
    val Empty = RegisterValues(values = Vector.fill(4)(0))
  }
  case class RegisterValues(values: Vector[Int]) {
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


  def matchingInstructions(sample: Sample): Set[InstructionDef] = instructions.filter { instructionDef =>
      sample.before.instruct(instructionDef, sample.args) == sample.after
    }.toSet

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
  val input2 = Source.fromResource("Day16-2-Input.txt").getLines().map { line =>
    val List(opcode, a, b, output) = line.split(' ').map(_.toInt).toList
    InstructionArgs(opcode, a, b, output)
  }.toList

  def part1(samples: List[Sample]): Int = input1.map(matchingInstructions).count(_.size >= 3)

  case class PossibleAssignments(opcodeToInstruction: Map[Int, Set[Symbol]]) {
    lazy val instructionToOpcode: Map[Symbol, Set[Int]] = {
      opcodeToInstruction.toList.foldLeft(Map.empty[Symbol, Set[Int]]){ case (agg, (opcode, symbols)) =>
        symbols.foldLeft(agg){(agg2, symbol) =>
          val currentOpcodes = agg2.getOrElse(symbol, Set.empty[Int])
          agg2.updated(symbol, currentOpcodes + opcode)
        }
      }
    }

    def remove(assignment: (Int, Symbol)): PossibleAssignments = {
      copy(opcodeToInstruction = (opcodeToInstruction - assignment._1).mapValues(symbols => symbols - assignment._2).filterNot { _._2.isEmpty })
    }

    def nextAssignmentWithSingleInstruction: Option[(Int, Symbol)] = {
      opcodeToInstruction.toList.collectFirst { case (opcode, symbols) if symbols.size == 1 => opcode -> symbols.head }
    }
    def nextAssignmentWithSingleOpcode: Option[(Int, Symbol)] = {
      instructionToOpcode.toList.collectFirst { case (symbol, opcodes) if opcodes.size == 1 => opcodes.head -> symbol }
    }
  }

  def part2(samples: List[Sample], testProgram: List[InstructionArgs]): Int = {
    val possibleAssignmentsMap: Map[Int, Set[Symbol]] = samples.foldLeft(Map.empty[Int, Set[Symbol]]){ (agg, sample) =>
      val currentDefs: Set[Symbol] = agg.getOrElse(sample.args.opcode, Set.empty[Symbol])
      val matchingDefs = matchingInstructions(sample).map(_.name)
      if (currentDefs.isEmpty) {
        agg.updated(sample.args.opcode, matchingDefs)
      } else {
        agg.updated(sample.args.opcode, currentDefs.intersect(matchingDefs))
      }
    }
    def eliminateRec(assignments: PossibleAssignments, assigned: Map[Int, Symbol]): Map[Int, Symbol] = {
      val nextAssignment = assignments.nextAssignmentWithSingleInstruction orElse assignments.nextAssignmentWithSingleOpcode
      nextAssignment match {
        case Some(assignment) => eliminateRec(assignments.remove(assignment), assigned + assignment)
        case None => assigned
      }
    }

    val opcodeMap = eliminateRec(PossibleAssignments(possibleAssignmentsMap), Map.empty).mapValues(symbol => instructions.find(_.name == symbol).get)
    assert(opcodeMap.keySet.size == opcodeMap.values.map(_.name).toSet.size)

    val endState = testProgram.foldLeft(RegisterValues.Empty){(registers, instruction) =>
      registers.instruct(opcodeMap(instruction.opcode), instruction)
    }
    endState.get(0)
  }

  println(part1(input1)) //529
  println(part2(input1, input2)) //573

}
