package com.clem.leopard.frontend

import com.clem.leopard.frontend.AST
import com.clem.leopard.frontend.AST.Values.Exp
import com.clem.leopard.frontend.AST.{Types, Values}
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

  override val skipWhitespace = true
  override val whiteSpace = WHITE_SPACE

  def id: Parser[String] = ID ^? (
    { case s if !reserved.contains(s) => s },
    { case s => s"RESERVED: $s" }
    )

  def lval: Parser[LVal] = simpleVar | fieldVar | subscriptVar | recordExp

  def simpleVar: Parser[SimpleVar] = id.map(SimpleVar)

  def subscriptVar: Parser[SubscriptVar] =
    for {
      lv <- lval
      xp <- "[" ~> expr <~ "]"
    } yield {
      SubscriptVar(lv, xp)
    }

  def fieldVar: Parser[FieldVar] =
    for {
      lv <- lval
      sym <- "." ~> id
    } yield {
      FieldVar(lv, sym)
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

  def fieldDec: Parser[(AST.Symbol, Type)] = id ~ ":" ~ ty ^^ {
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
    def op = "*" | "/" | "+" | "-" | "=" | "!=" | ">" | "<" | ">=" | "<=" | "&" | "|"
    def binApp = expr ~ op ~ expr ^^ {
      case l ~ o ~ r => CallExp(o, Seq(l, r))
    }
    def negate = "-" ~> expr ^^ { e => CallExp("-", Seq(e)) }
    def notApp = "!" ~> expr ^^ { e => CallExp("!", Seq(e)) }

    binApp | negate | notApp
  }

  def assignExp: Parser[Exp] = lval ~ ":=" ~ expr ^^ {
    case l ~ ":=" ~ v => AssignExp(l, v)
  }

  def arrayExp: Parser[Exp] =  id ~ "[" ~ expr ~ "]" ~ "of" ~ expr ^^ {
    case id ~ "[" ~ n ~ "]" ~ "of" ~ init if n.isInstanceOf[IntExp] || n.isInstanceOf[LVal] => ArrayExp(id, n, init)
  }

  def recordExp: Parser[LVal] = {
    def fieldCreate: Parser[(AST.Symbol, Exp)] = id ~ "=" ~ expr ^^ {
      case id ~ "=" ~ expr => (id, expr)
    }
    id ~ "{" ~ repsep(fieldCreate, ",") <~ "}" ^^ {
      case id ~ "{" ~ fields => RecordExp(id, fields)
    }
  }

  def expr: Parser[Exp] =
    lval | int | str | parenExp | letExp |
      seqExp | breakExp | ifExp |
      whileExp | forExp | letExp |
      opExp | arrayExp | recordExp |
      assignExp

  def parseString(s: String): Option[Exp] = {
    val input = new CharSequenceReader(s)
    TigerParser.expr(input) match {
      case TigerParser.Success(t,_) => Some(t)
      case _ => None
    }
  }
}
