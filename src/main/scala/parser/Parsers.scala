package parser

import parser.ParserCalc.{consumeMultipleAnyOf, consumeSingleAnyOf}
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
//  println(ParserCalc.parse(tokenize("a"), ParserCalc.parseIdentifier))
//  println(ParserCalc.parse(tokenize("ala9_2"), ParserCalc.parseIdentifier))
//  println(ParserCalc.parse(tokenize("ala9_2:"), ParserCalc.parseIdentifier))
//  println(ParserCalc.parse(tokenize(":ala9_2"), ParserCalc.parseIdentifier))
//  println
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

  println(ParserCalc.parse(tokenize("def myFunction(arg1:Int)="), ParserCalc.parseFunction)) //TODO
  println(ParserCalc.parse(tokenize("def myFunction(arg1:Int,arg2:Unit)="), ParserCalc.parseFunction))
  println(ParserCalc.parse(tokenize("def myFunction()="), ParserCalc.parseFunction))
  println(ParserCalc.parse(tokenize("def (arg1:Int,arg2:Unit)="), ParserCalc.parseFunction))
  println(ParserCalc.parse(tokenize("def myFunction="), ParserCalc.parseFunction))
  println(ParserCalc.parse(tokenize("def myFunction(Int arg1,Unit arg2)="), ParserCalc.parseFunction))
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
  val parseIdentifier: Parser = new ThenParser(consumeSingle[Letter], consumeMultipleAnyOf(classOf[Letter], classOf[Digit], Underscore.getClass))
  val parseType: Parser = consumeSingleAnyOf(UnitKeyword.getClass, IntKeyword.getClass)_
  val parseArg: Parser = new ThenParser(new ThenParser(parseIdentifier, consumeSingle[Colon.type]), parseType) //TODO double TODO
  val parseArgs: Parser = new ThenParser(new ThenParser(parseArg, consumeSingle[Comma.type]), parseArg)
  val parseFunction: Parser = new ThenParserN(
    consumeSingle[DefKeyword.type], consumeMultipleAnyOf(Whitespace.getClass)_, parseIdentifier,
    consumeSingle[LParenthesis.type], parseArgs, consumeSingle[RParenthesis.type],
    consumeSingle[Colon.type], parseType, consumeSingle[Equals.type], // TODO body
  )

  val recursive = new DelegatingParser()
  val parseExpression: Parser = or(
    new ThenParser(consumeSingle[LParenthesis.type], consumeSingle[RParenthesis.type]),
    new ThenParserN(consumeSingle[LCurly.type], recursive, consumeSingle[RCurly.type])
  )_
  recursive.targetParser = parseExpression // TODO

  // tools
  def parse(tokens: Seq[Token], parser: Parser): String = {
    parser.consume(tokens) match {
      case Some((parsed, Nil)) => s"SUCCESS: ${parsed}"
      case Some((parsed, rest)) => s"FAILURE: ${parsed} but left ${rest}"
      case None => "FAILURE"
    }
  }

  def or(parser1: Parser, parser2: Parser)(tokens: Seq[Token]): Option[(Seq[Token], Seq[Token])] = {
    parser1.consume(tokens) match {
      case Some(x) => Some(x)
      case None => parser2.consume(tokens)
    }
  }

  def consumeSingle[T](tokens: Seq[Token])(implicit tag: ClassTag[T]): Option[(Seq[Token], Seq[Token])] = tokens match {
    case head :: tail =>
      head match {
        case h@tag(_: T) => Some(Seq(h), tail)
        case _ => None
      }
    case _ => None
  }

  def consumeSingleAnyOf(cls: Class[_ <: Token]*)(tokens: Seq[Token]): Option[(Seq[Token], Seq[Token])] = tokens match {
    case head :: tail => cls.collectFirst({ case x if x == head.getClass => (Seq(head), tail) })
    case _ => None
  }

  def consumeMultipleAnyOf(cls: Class[_ <: Token]*)(tokens: Seq[Token]): Option[(Seq[Token], Seq[Token])] = //TODO parsers instead of classes as arg
    consumeMultipleAnyOfInternal(Seq.empty, tokens, cls)

  private def consumeMultipleAnyOfInternal(soFarConsumed: Seq[Token], tokens: Seq[Token], cls: Seq[Class[_ <: Token]]): Option[(Seq[Token], Seq[Token])] =
    tokens match {
      case head :: tail =>
        cls.collectFirst({ case x if x == head.getClass => (head, tail) })
          .flatMap(matchd => consumeMultipleAnyOfInternal(matchd._1 +: soFarConsumed, tail, cls))
          .orElse(Some(soFarConsumed.reverse, tokens))
      case Nil => Some(soFarConsumed.reverse, tokens)
    }

  }


abstract class Parser {
  def consume(tokens: Seq[Token]) : Option[(Seq[Token], Seq[Token])]
}

class DelegatingParser extends Parser {
  var targetParser: Parser = _
  override def consume(tokens: Seq[Token]): Option[(Seq[Token], Seq[Token])] = targetParser.consume(tokens)
}
/**
 * LEARNING - ClassTag, implicit, pattern match @ tag
 */

abstract class SingleParser extends Parser {
  type T <: Token
  def tag: ClassTag[T]
  override def consume(tokens: Seq[Token]): Option[(Seq[Token], Seq[Token])] = consumeInternal(tokens)(tag)
  def consumeInternal(tokens: Seq[Token])(/*OR implicit - that makes helluva difference*/ tag: ClassTag[T]): Option[(Seq[Token], Seq[Token])] = tokens match {
    case head::tail  => head match {
      case h @ tag(_: T) => Some(Seq(h), tail) /*OR h: T + implicit above  */
      case _ => None
    }
    case _ => None
  }
}

class ThenParser(parser1: Parser, parser2: Parser) extends Parser {
  override def consume(tokens: Seq[Token]): Option[(Seq[Token], Seq[Token])] = parser1.consume(tokens) //todo for, yield?
    .flatMap { parsed1 => {
      parser2.consume(parsed1._2).map(parsed2 => (parsed1._1 ++ parsed2._1, parsed2._2))
    }
    }
}

class ThenParserN(parsers: Parser*) extends Parser {
  override def consume(tokens: Seq[Token]): Option[(Seq[Token], Seq[Token])] = consumeInternal(tokens, parsers)
  def consumeInternal(tokens: Seq[Token], parsers: Seq[Parser]): Option[(Seq[Token], Seq[Token])] = {
    parsers match {
      case Seq() => Some(Seq.empty, tokens)
      case Seq(p, ps @ _*) =>
        p.consume(tokens)
          .flatMap(pres => {
//            consumeInternal(pres._2, ps) match {
//              case Some((parsed, remaining)) => Some((pres._1 ++ parsed, remaining))
//              case None => None
//            }
            consumeInternal(pres._2, ps).map(rest => (pres._1 ++ rest._1, rest._2))
          })

    }
  }
}

/**
 * Tokenization
 */
object Tokenizer {

  val keywords = List(IntKeyword, UnitKeyword)

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
object Whitespace extends Token
object Underscore extends Token
object Colon extends Token {
  override def toString: String = "Colon"
}
object Comma extends Token
object LParenthesis extends Token {
  override def toString: String = "("
}
object RParenthesis extends Token {
  override def toString: String = ")"
}
object Equals extends Token
object LCurly extends Token {
  override def toString: String = "{"
}
object RCurly extends Token {
  override def toString: String = "}"
}