package com.clem.leopard.frontend

import com.clem.leopard.frontend.AST.{Types, Values}
import scala.annotation.tailrec
import scala.util.parsing.combinator.{ImplicitConversions, RegexParsers}
import scala.util.parsing.input.CharSequenceReader

object Lexer {
  val INT_LIT = """\d+"""r
  val STR_LIT = """["]([^"\\\n\r]|\\(.|\n|\r))*\\?["]"""r
  val ID = """[a-zA-Z][\w_\d]*"""r
  val WHITE_SPACE = """\s*(/(\*([^*]|\*[^/])*\*/|/[^\n]*\n)\s*)*"""r

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

}

object TigerParser extends RegexParsers with ImplicitConversions {
  import Lexer._
  import Values._
  import Types._

  //override val skipWhitespace = true
  //override val whiteSpace = WHITE_SPACE

  def id: Parser[String] = ID ^? (
    { case s if !reserved.contains(s) => s },
    { case s => s"RESERVED: $s" }
    )

  def variableExp: Parser[LVal] = {
    sealed trait LValFragment
    case class Dot(sym: String, nxt: Option[LValFragment]) extends LValFragment
    case class Brackets(inside: Exp, nxt: Option[LValFragment]) extends LValFragment

    case class Ref(head: String, tail: Option[LValFragment]) {
      def exp: LVal = expandLvalFragmentTail(SimpleVar(head), tail)

      @tailrec
      final def expandLvalFragmentTail(head: LVal, tail: Option[LValFragment]): LVal = {
        if (tail.isEmpty) head
        else tail.get match {
          case Dot(s, t) => expandLvalFragmentTail(FieldVar(head, s), t)
          case Brackets(e, t) => expandLvalFragmentTail(SubscriptVar(head, e), t)
        }
      }
    }

    def fragment: Parser[LValFragment] = dot | brackets

    def dot: Parser[LValFragment] = "." ~> id ~ fragment.? ^^ {
      case id ~ ref => Dot(id, ref)
    }

    def brackets: Parser[LValFragment] = "[" ~> expr ~ "]" ~ fragment.? ^^ {
      case expr ~ "]" ~ refOpt => Brackets(expr, refOpt)
    }

    def ref: Parser[Ref] = id ~ fragment.? ^^ {
      case id ~ fragmentOpt => Ref(id, fragmentOpt)
    }

    ref.map(_.exp)
}

  def parenExp: Parser[Exp] = "(" ~> expr <~ ")"

  def int: Parser[Exp] = INT_LIT ^^ { s => IntExp(s.toInt) }

  def str: Parser[Exp] = STR_LIT ^^ { s => StringExp(s) }

  def seqExp: Parser[Exp] = "(" ~> repsep(expr, ";") <~ ")" ^^ { SeqExp(_) }

  def breakExp: Parser[Break.type] = "break" ^^ { _ => Break }

  def ifExp: Parser[Exp] = "if" ~ expr ~ "then" ~ expr ~ ("else" ~ expr).? ^^ {
    case "if" ~ p ~ "then" ~ c ~ Some("else" ~ a) => IfExp(p, c, Some(a))
    case "if" ~ p ~ "then" ~ c ~ None => IfExp(p, c, None)
  }

  def whileExp: Parser[Exp] = "while" ~ expr ~ "do" ~ expr ^^ {
    case "while" ~ p ~ "do" ~ b => WhileExp(p, b)
  }

  def forExp: Parser[Exp] = "for" ~ id ~ ":=" ~ expr ~ "to" ~ expr ~ "do" ~ expr ^^ {
    case "for" ~ id ~ ":=" ~ init ~ "to" ~ term ~ "do" ~ body =>
      ForExp(id, init, term, body)
  }

  def fieldDec: Parser[(String, Type)] = id ~ ":" ~ ty ^^ {
    case id ~ ":" ~ ty => (id, ty)
  }

  def ty: Parser[Type] = {
    def idT: Parser[Type] = id.map(NameType)
    def arrT: Parser[Type] = "array" ~> "of" ~> ty ^^ {
      ArrayType
    }
    def recT: Parser[Type] = "{" ~> repsep(fieldDec, ",") <~ "}" ^^ RecordType

    idT | arrT | recT
  }

  def dec: Parser[LetDec] = {
    def varLetDec: Parser[LetDec] = "var" ~> id ~ (":" ~> ty).? ~ ":=" ~ expr ^^ {
      case id ~ tyOpt ~ ":=" ~ expr => VarLetDec(id, tyOpt, expr)
    }
    def funDec: Parser[LetDec] = "function" ~> id ~ "(" ~ repsep(fieldDec, ",") ~ ")" ~ (":" ~> ty).? ~ "=" ~ expr ^^ {
      case id ~ "(" ~ args ~ ")" ~ returnType ~ "=" ~ body => FunDec(id, args, returnType, body)
    }
    def typeDec: Parser[LetDec] = "type" ~ id ~ "=" ~ ty ^^ {
      case "type" ~ id ~ "=" ~ ty => TypeDec(id, ty)
    }

    varLetDec | funDec | typeDec
  }

  def letExp: Parser[Exp] = "let" ~> rep1(dec) ~ "in" ~ rep1sep(expr, ";") <~ "end" ^^ {
    case ds ~ "in" ~ es => LetExp(ds, es)
  }

  def opExp: Parser[Exp] = {
    sealed trait Assoc

    val ops = Map(
      (1, Set("*", "/")),
      (2, Set("-", "+")),
      (3, Set("=", "!=", ">", "<", ">=", "<=")),
      (4, Set("&")),
      (5, Set("|"))
    )

    def opsWithPrecedence(n: Int): Set[String] = ops.getOrElse(n, Set.empty)

    def opWithPrecedence(n: Int): Parser[String] = {
      val ops = opsWithPrecedence(n)
      if (ops.size > 1) {
        ops.map(literal).fold (literal(ops.head)) {
          case (l1, l2) => l1 | l2
        }
      } else if (ops.size == 1) {
        literal(ops.head)
      } else {
        failure(s"No Ops for Precedence $n")
      }
    }

    def folder(h: Exp, t: TigerParser.~[String, Exp]): CallExp = CallExp(t._1, Seq(h, t._2))

    val maxPrecedence: Int = ops.maxBy(_._1)._1

    def negate = "-" ~> expr ^^ { e => CallExp("-", Seq(e)) }

    def notApp = "!" ~> expr ^^ { e => CallExp("!", Seq(e)) }

    def term: (Int => Parser[Exp]) = {
      case 0 => callExp | lval | int | negate | notApp | "(" ~> expr <~ ")"
      case n if n > 0 =>
        val lowerTerm = term(n - 1)
        lowerTerm ~ rep(opWithPrecedence(n) ~ lowerTerm) ^^ {
          case h ~ ts if ts.nonEmpty => ts.foldLeft(h)(folder)
          case h ~ _ => h
        }
    }

    term(maxPrecedence)
  }

  def assignExp: Parser[Exp] = variableExp ~ ":=" ~ expr ^^ {
    case l ~ ":=" ~ v => AssignExp(l, v)
  }

  def arrayExp: Parser[Exp] =  id ~ "[" ~ expr ~ "]" ~ "of" ~ expr ^^ {
    case id ~ "[" ~ n ~ "]" ~ "of" ~ init => ArrayExp(id, n, init)
  }

  def recordExp: Parser[LVal] = {
    def fieldCreate: Parser[(String, Exp)] = id ~ "=" ~ expr ^^ {
      case id ~ "=" ~ expr => (id, expr)
    }
    id ~ "{" ~ repsep(fieldCreate, ",") <~ "}" ^^ {
      case id ~ "{" ~ fields => RecordExp(id, fields)
    }
  }
  
  def lval: Parser[LVal] = recordExp | variableExp

  def callExp: Parser[Exp] = id ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case fn ~ "(" ~ args ~ ")" => CallExp(fn, args)
  }

  def expr: Parser[Exp] = opExp | assignExp | callExp |
    int | str | breakExp | arrayExp | seqExp | lval |
    ifExp | whileExp | forExp | parenExp

  def parseString(s: String): Option[Exp] = {
    val input = new CharSequenceReader(s)
    TigerParser.parseAll(expr, input) match {
      case TigerParser.Success(h, _) => Some(h)
      case _ => None
    }
  }
}
