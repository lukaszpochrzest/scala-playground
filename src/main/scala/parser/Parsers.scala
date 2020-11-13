package parser

import parser.ParserCalc.{consumeMultipleAnyOf, consumeSingleAnyOf}
import parser.Tokenizer.tokenize

import scala.reflect.ClassTag

object Parsers extends App {
  println(consumeSingleAnyOf(classOf[Letter])(tokenize("a")))
  println(consumeSingleAnyOf(classOf[Letter])(tokenize(":")))
  println(consumeSingleAnyOf(Colon.getClass)(tokenize("a")))
  println(consumeSingleAnyOf(Colon.getClass)(tokenize(":")))
  println(consumeSingleAnyOf(Colon.getClass)(tokenize(":a")))
  println

  println(consumeMultipleAnyOf(classOf[Letter])(tokenize("aaaaa::")))
  println(consumeMultipleAnyOf(classOf[Letter])(tokenize(":aaaaa")))
  println(consumeMultipleAnyOf(Colon.getClass)(tokenize(":aaaaa")))
  println(consumeMultipleAnyOf(Colon.getClass, classOf[Letter])(tokenize(":aaaaa")))
  println(consumeMultipleAnyOf(Colon.getClass, classOf[Letter])(tokenize(":aaaaa_")))
  println

  println(ParserCalc.parse(tokenize("ala9_2"), ParserCalc.parseIdentifier))
  println(ParserCalc.parse(tokenize("ala9_2:"), ParserCalc.parseIdentifier))
  println(ParserCalc.parse(tokenize(":ala9_2"), ParserCalc.parseIdentifier))
  println

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
  val parseKeyword: Parser = consumeSingleAnyOf(Underscore.getClass)_

  // tools
  def parse(tokens: Seq[Token], parser: Parser): String = {
    parser.consume(tokens) match {
      case Some((parsed, Nil)) => s"SUCCESS: ${parsed}"
      case Some((parsed, rest)) => s"FAILURE: ${parsed} but left ${rest}"
      case None => "FAILURE"
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
object IntKeyword extends Token {
  override def toString: String = "Int"
}
object UnitKeyword extends Token {
  override def toString: String = "Int"
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
object LParenthesis extends Token
object RParenthesis extends Token
object Equals extends Token
object LCurly extends Token
object RCurly extends Token
