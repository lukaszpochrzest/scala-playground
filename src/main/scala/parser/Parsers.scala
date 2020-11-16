package parser

import parser.Tokenizer.tokenize

import scala.reflect.ClassTag

object Parsers extends App {
//  println(consumeSingleAnyOf(classOf[Letter])(tokenize("a")))
//  println(consumeSingleAnyOf(classOf[Letter])(tokenize(":")))
//  println(consumeSingleAnyOf(Colon.getClass)(tokenize("a")))
//  println(consumeSingleAnyOf(Colon.getClass)(tokenize(":")))
//  println(consumeSingleAnyOf(Colon.getClass)(tokenize(":a")))
//  println
//
//  println(consumeMultipleAnyOf(classOf[Letter])(tokenize("aaaaa::")))
//  println(consumeMultipleAnyOf(classOf[Letter])(tokenize(":aaaaa")))
//  println(consumeMultipleAnyOf(Colon.getClass)(tokenize(":aaaaa")))
//  println(consumeMultipleAnyOf(Colon.getClass, classOf[Letter])(tokenize(":aaaaa")))
//  println(consumeMultipleAnyOf(Colon.getClass, classOf[Letter])(tokenize(":aaaaa_")))
//  println
//
  println(ParserCalc.parse(tokenize("a"), ParserCalc.parseIdentifier))
  println(ParserCalc.parse(tokenize("ala9_2"), ParserCalc.parseIdentifier))
  println(ParserCalc.parse(tokenize("ala9_2:"), ParserCalc.parseIdentifier))
  println(ParserCalc.parse(tokenize(":ala9_2"), ParserCalc.parseIdentifier))
  println
//
//  println(ParserCalc.parse(tokenize("Unit"), ParserCalc.parseType))
//  println(ParserCalc.parse(tokenize("Int"), ParserCalc.parseType))
//  println(ParserCalc.parse(tokenize("Unit9"), ParserCalc.parseType))
//  println(ParserCalc.parse(tokenize("Un:t"), ParserCalc.parseType))
//  println
//
//  println(ParserCalc.parse(tokenize("argName:Unit"), ParserCalc.parseArg))
//  println(ParserCalc.parse(tokenize("argName:Int"), ParserCalc.parseArg))
//  println(ParserCalc.parse(tokenize("argName :Int"), ParserCalc.parseArg))
//  println(ParserCalc.parse(tokenize("argName"), ParserCalc.parseArg))
//  println(ParserCalc.parse(tokenize("argName:"), ParserCalc.parseArg))
//  println(ParserCalc.parse(tokenize(":Unit"), ParserCalc.parseArg))
//  println(ParserCalc.parse(tokenize(":Int"), ParserCalc.parseArg))
//  println(ParserCalc.parse(tokenize("Int"), ParserCalc.parseArg))
//  println(ParserCalc.parse(tokenize("a:a"), ParserCalc.parseArg))
//  println

  println(ParserCalc.parse(tokenize("def myFunction(arg1:Int,arg2:Unit):Unit="), ParserCalc.parseFunction)) //TODO
  println(ParserCalc.parse(tokenize("def myFunction(arg1:Int,arg2:Unit,arg3:Unit,arg3:Int):Unit="), ParserCalc.parseFunction)) //TODO
  println(ParserCalc.parse(tokenize("def myFunction(arg1:Int):Unit="), ParserCalc.parseFunction)) //TODO
  println(ParserCalc.parse(tokenize("def myFunction():Unit="), ParserCalc.parseFunction))
  println(ParserCalc.parse(tokenize("def (arg1:Int,arg2:Unit):Unit="), ParserCalc.parseFunction))
  println(ParserCalc.parse(tokenize("def myFunction:Unit="), ParserCalc.parseFunction))
  println(ParserCalc.parse(tokenize("def myFunction(Int arg1,Unit arg2):Unit="), ParserCalc.parseFunction))
  println

//  println(ParserCalc.parse(tokenize("{{{()}}}"), ParserCalc.parseExpression))
//  println(ParserCalc.parse(tokenize("{{{()}}}()"), ParserCalc.parseExpression))
//  println(ParserCalc.parse(tokenize("{{{}}}"), ParserCalc.parseExpression))
//  println(ParserCalc.parse(tokenize("{{{(}}}"), ParserCalc.parseExpression))
//  println(ParserCalc.parse(tokenize("{{{()}}"), ParserCalc.parseExpression))
//  println(ParserCalc.parse(tokenize("{{()}}}"), ParserCalc.parseExpression))
//  println




  //
//  val tokens = tokenize("q:")
//  println(tokens)
////  val parseResult = ParserCalc.parse(tokens, new ThenParser(
////    new SingleParser{
////      type T = Letter
////      def tag = implicitly
////    },
////    new SingleParser {
////      type T = Colon.type
////      def tag = implicitly
////    }))
//

  //
//  val tokens = tokenize(":qq")
//  val parseResult = ParserCalc.parse(tokens, new ThenParser(consumeSingle[Colon.type], consumeSingle[Letter]))
//  println(parseResult)
}



///**
// * Identifier
// */
//object Identifier {
//  private def isIdentifier(tokens: Seq[Token]): Boolean = {
//    tokens.head.isInstanceOf[Letter] && !tokens.tail.exists(s => !s.isInstanceOf[Letter] && !s.isInstanceOf[Digit] && s != Underscore)
//  }
//
//  def consumeSingle[T](token: Token, value: Class[T]): Boolean = token.isInstanceOf[T]
//  def startWith[T](token: Token, value: Class[T]): Boolean = token.isInstanceOf[T]
//}
//class Identifier(val tokens: Seq[Token])

object ParserCalc {
  //
  // rules
  val parseIdentifier: Parser = thenConsume(consumeSingle[Letter], consumeMultipleAnyOf(consumeSingle[Letter], consumeSingle[Digit], consumeSingle[Underscore.type])_)_
  val parseType: Parser = consumeSingleAnyOf(UnitKeyword.getClass, IntKeyword.getClass)_
  val parseArg: Parser = thenConsume(parseIdentifier, consumeSingle[Colon.type], parseType)_
  val parseArgs: Parser = thenConsume(parseArg, consumeMultipleAnyOf(thenConsume(consumeSingle[Comma.type], parseArg)_)_)_
  val parseFunction: Parser = thenConsume(
    consumeSingle[DefKeyword.type], consumeMultipleAnyOf(consumeSingle[Whitespace.type])_, parseIdentifier,
    consumeSingle[LParenthesis.type], parseArgs, consumeSingle[RParenthesis.type],
    consumeSingle[Colon.type], parseType, consumeSingle[Equals.type], // TODO body
  )_

  val recursive = new DelegatingParser()
  val parseExpression: Parser = or(
    thenConsume(consumeSingle[LParenthesis.type], consumeSingle[RParenthesis.type])_,
    thenConsume(consumeSingle[LCurly.type], recursive, consumeSingle[RCurly.type])_
  )_
  recursive.targetParser = parseExpression // TODO

  // tools
  def parse(tokens: Seq[Token], parser: Parser): String = {
    parser.consume(tokens) match {
      case Some(ParserResult(parsed, Nil)) => s"SUCCESS: ${parsed}"
      case Some(ParserResult(parsed, rest)) => s"FAILURE: ${parsed} but left ${rest}"
      case None => "FAILURE"
    }
  }

  def or(parser1: Parser, parser2: Parser)(tokens: Seq[Token]): Option[ParserResult] = {
    parser1.consume(tokens) match {
      case Some(x) => Some(x)
      case None => parser2.consume(tokens)
    }
  }

  def consumeSingle[T](tokens: Seq[Token])(implicit tag: ClassTag[T]): Option[ParserResult] = tokens match {
    case head :: tail =>
      head match {
        case h@tag(_: T) => Some(ParserResult(Seq(h), tail))
        case _ => None
      }
    case _ => None
  }

  def consumeSingleAnyOf(cls: Class[_ <: Token]*)(tokens: Seq[Token]): Option[ParserResult] = tokens match {
    case head :: tail => cls.collectFirst({ case x if x == head.getClass => ParserResult(Seq(head), tail) })
    case _ => None
  }

  // optional
  def consumeMultipleAnyOf(parsers: Parser*)(tokens: Seq[Token]): Option[ParserResult] =
    consumeMultipleAnyOfInternal(Seq.empty, tokens, parsers)

  private def consumeMultipleAnyOfInternal(soFarConsumed: Seq[Token], tokens: Seq[Token], parsers: Seq[Parser]): Option[ParserResult] =
    parsers.view
      .flatMap(_.consume(tokens).flatMap(pResult => consumeMultipleAnyOfInternal(soFarConsumed ++ pResult.parsed, pResult.notParsed, parsers)))// TODO
      .headOption
      .orElse(Some(ParserResult(soFarConsumed, tokens)))

  def thenConsume(parsers: Parser*)(tokens: Seq[Token]): Option[ParserResult] = {
    parsers match {
      case Seq() => Some(ParserResult(Seq.empty, tokens))
      case Seq(p, ps @ _*) =>
        p.consume(tokens) // TODO for, yield?
          .flatMap(pres => thenConsume(ps:_*)(pres.notParsed).map(rest => ParserResult(pres.parsed ++ rest.parsed, rest.notParsed)))
    }
  }

}

case class ParserResult(parsed: Seq[Token], notParsed: Seq[Token])

abstract class Parser {
  def consume(tokens: Seq[Token]) : Option[ParserResult]
}

class DelegatingParser extends Parser {
  var targetParser: Parser = _
  override def consume(tokens: Seq[Token]): Option[ParserResult] = targetParser.consume(tokens)
}
/**
 * LEARNING - ClassTag, implicit, pattern match @ tag
 */

abstract class SingleParser extends Parser {
  type T <: Token
  def tag: ClassTag[T]
  override def consume(tokens: Seq[Token]): Option[ParserResult] = consumeInternal(tokens)(tag)
  def consumeInternal(tokens: Seq[Token])(/*OR implicit - that makes helluva difference*/ tag: ClassTag[T]): Option[ParserResult] = tokens match {
    case head::tail  => head match {
      case h @ tag(_: T) => Some(ParserResult(Seq(h), tail)) /*OR h: T + implicit above  */
      case _ => None
    }
    case _ => None
  }
}

/**
 * Tokenization
 */
object Tokenizer {

  val keywords = List(IntKeyword, UnitKeyword, DefKeyword)

  def tokenize(input: String): List[Token] = {
    if(input.isEmpty)
      List.empty
    else {
      val keywordPrefix =  keywords.find(keyword => input.startsWith(keyword.toString))
      if(keywordPrefix.isEmpty) {
        val token = mapChar(input.charAt(0))
        val unParsed = input.drop(1)
        token :: tokenize(unParsed)
      } else {
        val unParsed = input.stripPrefix(keywordPrefix.get.toString)
        keywordPrefix.get :: tokenize(unParsed)
      }
    }

//        .flatMap(keyword => {
//          val unParsed = input.stripPrefix(keyword.toString)
//          keyword +: tokenize(unParsed)
//        })
//        .orElse({
//          val token = mapChar(input.charAt(0))
//          val unParsed = input.drop(1)
//          return token +: tokenize(unParsed)
//        })
  }

  def mapChar(c : Char): Token = c match {
    case s if s.isLetter => new Letter(s)
    case s if s.isDigit => new Digit(s)
    case ' ' => Whitespace
    case '_' => Underscore
    case ':' => Colon
    case ',' => Comma
    case ')' => RParenthesis
    case '(' => LParenthesis
    case '=' => Equals
    case '}' => RCurly
    case '{' => LCurly
    case unknown => throw new IllegalArgumentException("Unknown symbol " + unknown)
  }

}

class Token
object DefKeyword extends Token {
  override def toString: String = "def"
}
object IntKeyword extends Token {
  override def toString: String = "Int"
}
object UnitKeyword extends Token {
  override def toString: String = "Unit"
}
class Letter(val value:Char) extends Token {
  override def toString: String = s"Letter(${value})"
}
class Digit(val value:Char) extends Token {
  override def toString: String = s"Digit(${value})"
}
object Whitespace extends Token {
  override def toString: String = " "
}
object Underscore extends Token
object Colon extends Token {
  override def toString: String = ":"
}
object Comma extends Token
object LParenthesis extends Token {
  override def toString: String = "("
}
object RParenthesis extends Token {
  override def toString: String = ")"
}
object Equals extends Token {
  override def toString: String = "="
}
object LCurly extends Token {
  override def toString: String = "{"
}
object RCurly extends Token {
  override def toString: String = "}"
}