package com.clem.leopard.frontend

import org.apache.commons.lang.StringEscapeUtils
import com.clem.leopard.frontend.AST.Types.Type


object AST
{
  type Symbol = String

  object Values {
    sealed trait Exp extends Showable {

      def showFieldVal(fieldVal: (AST.Symbol, Exp)): String = fieldVal match {
        case (sym, exp) => s"$sym = ${exp.show}"
      }

      def show: String = this match {
        case IntExp(value) => value.toString
        case StringExp(value) => StringEscapeUtils.escapeJava(value)
        case CallExp(fn, args) => s"$fn(${args.map(_.show).mkString(",")})"
        case RecordExp(recType, args) => s"$recType{${args.map(showFieldVal).mkString(",")}}"
        case Break => "break"
        case Null => "null"
        case SeqExp(exps) => s"(${exps.map(_.show).mkString(";")})"
        case AssignExp(lval, rval) => s"${lval.show} := ${rval.show}"
        case IfExp(predicate, consequent, Some(alternate)) => s"if ${predicate.show} then ${consequent.show} else ${alternate.show}"
        case IfExp(predicate, consequent, None) => s"if ${predicate.show} then ${consequent.show}"
        case WhileExp(predicate, body) => s"while ${predicate.show} do ${body.show}"
        case ForExp(id, init, term, body) => s"for $id := ${init.show} to ${term.show} do ${body.show}"
        case LetExp(decs, body) => s"let ${decs.map(_.show).mkString("\n")} in ${body.map(_.show).mkString(";")} end"
        case ArrayExp(typ, n, init) => s"$typ[${n.show}] of ${init.show}"
        case SimpleVar(name) => name
        case FieldVar(lval, field) => s"${lval.show}.${field}"
        case SubscriptVar(lval, subscript) => s"${lval.show}[${subscript.show}]"
      }
    }

    sealed trait LVal extends Exp

    type Program = Exp

    type Predicate = Exp
    type Body = Exp

    case class IntExp(value: Int) extends Exp
    case class StringExp(value: String) extends Exp
    case class CallExp(fn: Symbol, args: Seq[Exp]) extends Exp
    case class RecordExp(recType: Symbol, args: Seq[(Symbol, Exp)]) extends Exp with LVal
    case object Break extends Exp
    case object Null extends Exp
    case class SeqExp(exps: Seq[Exp]) extends Exp
    case class AssignExp(lval: LVal, rval: Exp) extends Exp
    case class IfExp(predicate: Exp, consequent: Exp, alternate: Option[Exp]) extends Exp
    case class WhileExp(prediate: Exp, body: Exp) extends Exp
    case class ForExp(id: Symbol, init: Exp, term: Exp, body: Exp) extends Exp
    case class LetExp(decs: Seq[LetDec], body: Seq[Exp]) extends Exp
    case class ArrayExp(typ: Symbol, n: Exp, init: Exp) extends Exp

    case class SimpleVar(name: Symbol) extends LVal
    case class FieldVar(lval: LVal, field: Symbol) extends LVal
    case class SubscriptVar(lval: LVal, subscript: Exp) extends LVal

    sealed trait LetDec {
      def show: String = this match {
        case VarLetDec(name, Some(typ), value) => s"var $name : ${typ.show} := ${value.show}"
        case VarLetDec(name, None, value) => s"var $name := ${value.show}"
        case FunDec(name, args, Some(returnType), body) => s"function $name(${args.map(showFieldSpec).mkString(",")}): ${returnType.show} = ${body.show}"
        case FunDec(name, args, None, body) => s"function $name(${args.map(showFieldSpec).mkString(",")}) = ${body.show}"
       case TypeDec(name, value) => s"type $name = ${value.show}"
      }
    }

    case class VarLetDec(name: Symbol, typ: Option[Types.Type], value: Exp) extends LetDec
    case class FunDec(name: Symbol, args: Seq[(Symbol, Types.Type)], returnType: Option[Types.Type], body: Exp) extends LetDec
    case class TypeDec(name: Symbol, value: Types.Type) extends LetDec

  }

  object Types {
    sealed trait Type extends Showable  {
      def show: String = this match {
        case NameType(name) => name
        case ArrayType(values) => values + "[]"
        case RecordType(fields) => s"{${fields.map(showFieldSpec).mkString(",")}}"
        case FunctionType(args, returns) => s"((${args.map(_.show).mkString(",")}}) -> ${returns.show}})"
      }
    }

    case class NameType(name: Symbol) extends Type
    case class ArrayType(values: Type) extends Type
    case class RecordType(fields: Seq[(Symbol, Type)]) extends Type
    case class FunctionType(args: Seq[Type], returns: Type) extends Type
  }

  def showFieldSpec(fieldSpec: (AST.Symbol, Type)): String = fieldSpec match {
    case (sym, ty) => s"$sym : ${ty.show}"
  }

  trait Showable {
    def show: String
  }
}
