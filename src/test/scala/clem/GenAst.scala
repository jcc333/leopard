package clem

import org.scalacheck.Gen
import org.scalacheck._
import Gen._

import scala.clem.frontend.AST.Types.{RecordType, ArrayType, NameType, Type}
import scala.clem.frontend.AST.Values._
import scala.clem.frontend.AST._

object GenAst {
  implicit val arbitraryExpression = Arbitrary(genExp)

  def genExp: Gen[Exp] = {
    def int: Gen[Exp] = Gen.chooseNum(Int.MinValue, Int.MaxValue).map(IntExp)

    def str: Gen[Exp] = Arbitrary.arbString.arbitrary.map(StringExp)

    def letExp: Gen[Exp] = {
      def dec = oneOf(varDec, funDec, typeDec)

      def varDec = for {
        name <- Gen.identifier
        typ <- Gen.option(typeGen)
        value <- genExp
      } yield VarLetDec(name, typ, value)

      def funDec = for {
        name <- Gen.identifier
        args <- Gen.listOf(fieldSpec)
        returnType <- Gen.option(typeGen)
        body <- genExp
      } yield FunDec(name, args, returnType, body: Exp)

      def typeDec = for {
        name <- Gen.identifier
        typ <- typeGen
      } yield TypeDec(name, typ)

      for {
        decs <- Gen.listOf(dec)
        body <- Gen.listOf(genExp)
      } yield LetExp(decs, body)
    }


    def seqExp: Gen[Exp] = Gen.listOf(genExp).map(SeqExp)

    def breakExp: Gen[Exp] = Break

    def ifExp: Gen[Exp] = for(p <- genExp; c <- genExp; a <- Gen.option(genExp)) yield IfExp(p, c, a)

    def whileExp: Gen[Exp] = for(p <- genExp; b <- genExp) yield WhileExp(p, b)

    def forExp: Gen[Exp] = for {
      id <- Gen.identifier
      init <- genExp
      term <- genExp
      body <-genExp
    } yield ForExp(id, init, term, body)

    def arrayExp: Gen[Exp] = for {
      t <- Gen.identifier
      n <- genExp
      init <- genExp
    } yield ArrayExp(t, n, init)

    def recordExp: Gen[LVal] = for {
      t <- Gen.identifier
      args <- Gen.listOf {
        for {
          fld <- Gen.identifier
          exp <- genExp
        } yield (fld, exp)
      }
    } yield RecordExp(t, args)

    def assignExp: Gen[Exp] = for {
      lv <- lval
      rv <- genExp
    } yield AssignExp(lv, rv)

    def lval: Gen[LVal] = {
      def simpleVar = Gen.identifier.map(SimpleVar)

      def fieldVar = for {
        lv <- lval
        fld <- Gen.identifier
      } yield FieldVar(lv, fld)

      def subscriptVar = for {
        lv <- lval
        sb <- genExp
      } yield SubscriptVar(lv, sb)

      Gen.oneOf(simpleVar, fieldVar, subscriptVar, recordExp)
    }
    Gen.oneOf(
      int,
      str,
      letExp,
      seqExp,
      breakExp,
      ifExp,
      whileExp,
      forExp,
      arrayExp,
      recordExp,
      assignExp,
      lval
    )
  }

  def fieldSpec = for {
    symbol <- Gen.identifier
    typ <- typeGen
  } yield (symbol, typ)

  def typeGen: Gen[Type] = {
    def nameType = Gen.identifier.map(NameType)

    def arrayType = typeGen.map(ArrayType)

    def recordType = Gen.listOf(fieldSpec).map(RecordType)

    Gen.oneOf(nameType, arrayType, recordType)
  }

}
