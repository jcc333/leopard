package scala.clem.frontend
import scala.clem.frontend.AST._
import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader


object AST
{
  type Symbol = String
  class Lexer extends RegexParsers with ImplicitConversions {

    private def toSym(s: String): Symbol = s

    override val whiteSpace = """\s*(/(\*([^*]|\*[^/])*\*/|/[^\n]*\n)\s*)*"""r
    override val skipWhitespace = true

    val reserved = Set(
      "while",
      "for",
      "to",
      "break",
      "let",
      "in",
      "end",
      "function",
      "var",
      "type",
      "array",
      "if",
      "then",
      "else",
      "do",
      "of",
      "nil"
    )

    /** Keywords */

    val WHILE = "while" ^^ toSym
    val FOR = "for" ^^ toSym
    val TO = "to" ^^ toSym
    val BREAK = "break" ^^ toSym
    val LET = "let" ^^ toSym
    val IN = "in" ^^ toSym
    val END = "end" ^^ toSym
    val FUNCTION = "function" ^^ toSym
    val VAR = "var" ^^ toSym
    val TYPE = "type" ^^ toSym
    val ARRAY = "array" ^^ toSym
    val IF = "if" ^^ toSym
    val THEN = "then" ^^ toSym
    val ELSE = "else" ^^ toSym
    val DO = "do" ^^ toSym
    val OF = "of" ^^ toSym
    val NIL = "nil" ^^ toSym

    /** Operators */

    val PLUS = "+" ^^ toSym
    val MINUS = "-" ^^ toSym
    val TIMES = "*" ^^ toSym
    val DIVIDE = "/" ^^ toSym
    val EQ = "=" ^^ toSym
    val NEQ = "<>" ^^ toSym
    val LT = "<" ^^ toSym
    val LE = "<=" ^^ toSym
    val GT = ">" ^^ toSym
    val GE = ">=" ^^ toSym
    val AND = "&" ^^ toSym
    val OR = "|" ^^ toSym
    val ASSIGN = ":=" ^^ toSym
    val DOT = "." ^^ toSym

    /** Punctuation Symbols */
    val LBRACE = "{" ^^ toSym
    val RBRACE = "}" ^^ toSym
    val LPAREN = "(" ^^ toSym
    val RPAREN = ")" ^^ toSym
    val LBRACK = "[" ^^ toSym
    val RBRACK = "]" ^^ toSym
    val COLON = ":" ^^ toSym

    /** Integer Literals */
    val INT_LIT = ("""\d+""".r) ^^ { _ toInt }

    /** String Literals */
    val STR_LIT = ("""["]([^"\\\n\r]|\\(.|\n|\r))*\\?["]"""r) ^^ { e => e}

    /** Identifiers */

    val ID : Parser[Symbol] = ("""[a-zA-Z][\w_\d]*"""r) ^^ toSym
  }

  trait TigerParsers extends RegexParsers {
    import scala.clem.frontend.AST.Lexer
    import Values._
    import Types._

    val symbol = """[a-zA-Z][\w_\d]*"""r ^^ { s => s}

      def int: Parser[Exp] = """\d+""".r ^^ {
        s => IntExp(s.toInt)
      }

      def parenthesizedExpression: Parser[Exp] = "(" ~> expression <~ ")"


    def expression: Parser[Exp] = int | parenthesizedExpression
  }

  object Values {

    sealed trait Exp

    trait LiteralExp[T] extends Exp {
      def value: T
    }

    type Program = Exp

    type Predicate = Exp
    type Body = Exp

    case class IntExp(value: Int) extends LiteralExp[Int]
    case class StringExp(value: String) extends LiteralExp[String]
    case class CallExp(fn: Symbol, args: Seq[Exp]) extends Exp
    case class RecordExp(recType: Symbol, args: Seq[(Symbol, Exp)]) extends Exp
    case object Break extends Exp
    case object Null extends Exp
    case class SeqExp(exps: Seq[Exp]) extends Exp
    case class AssignExp(lval: Lvalue, rval: Exp) extends Exp
    case class IfExp(predicate: Exp, consequent: Exp, alternate: Option[Exp]) extends Exp
    case class WhileExp(prediate: Exp, body: Exp) extends Exp
    case class ForExp(init: Symbol, update: Exp, predicate: Exp, body: Exp) extends Exp
    case class LetExp(decs: Seq[LetDec], body: Exp) extends Exp
    case class ArrayExp(typ: Symbol, n: Exp, init: Exp) extends Exp

    sealed trait LValue
    case class SimpleVar(name: Symbol) extends LValue with Exp
    case class FieldVar(lval: LValue, field: Symbol) extends LValue with Exp
    case class SubscriptVar(lval: LValue, subscript: Exp) extends LValue with Exp

    trait LetDec
    case class VarLetDec(name: Symbol, typ: Option[Symbol], value: Exp)
    case class FunSpec(name: Symbol, args: Seq[(Symbol, Types.Type)], returnType: Option[Symbol], body: Exp) extends LetDec
    case class TypeSpec(name: Symbol, value: Types.Type)
    case class TypeDecs(specs: Seq[TypeSpec]) extends LetDec
  }

  object Types {
    trait Type
    case class NameType(name: Symbol) extends Type
    case class FieldSpec(name: Symbol, ty: Type, idx: Int)
    case class ArrayType(values: Type) extends Type
    case class RecordType(fields: Seq[FieldSpec])
    case class FunctionType(args: Seq[Type], returns: Type)
  }
}
