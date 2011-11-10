#!/bin/sh
exec scala -savecompiled "$0" "$@"
!#

// Simple preprocessor for XML: replaces open/close tags by more convenient
// syntax niceties (indentation, curly brackets)

// 'tag<attributes>:' or 'tag:', ruled by indentation:
//  - same indent: next block
//  - more indent: within block
// 'tag<attributes>{'...'}' or 'tag{'...'}', explicit contents
// '{{'..'}}' verbatim

import scala.util.parsing.combinator._

sealed abstract class Token
case class Indent(value:Int) extends Token
case class Text(contents:String) extends Token
case object ClosingBrace extends Token
abstract case class Tag(name:String, attrs:Option[String]) extends Token
class IndentTag(name:String, attrs:Option[String]) extends Tag(name, attrs)
class BraceTag(name:String, attrs:Option[String]) extends Tag(name, attrs)

object IndentLexer extends RegexParsers with ImplicitConversions {
  override val whiteSpace = "".r

  def indent : Parser[Token] = "[ \t]*".r ^^ (s => Indent(s.length))

  def tag : Parser[Token] =
    """[a-zA-Z][a-zA-Z0-9_-]*""".r ~
    opt("<" ~> "[^>]*".r <~ ">") ~
    (":" | "{") ^^ {
      case tag ~ args ~ ":" => new IndentTag(tag, args)
      case tag ~ args ~ "{" => new BraceTag(tag, args)
    }

  def control : Parser[Token] = not("}}") ~> "}" ^^^ ClosingBrace

  def verbatim : Parser[Token] = "{{" ~> """([^}]*(}[^}])?)+""".r <~ "}}" ^^ (s => Text(s))
  // The regexp could be replaced by (rep(not("}}") ~> ".|\n".r) ^^ _.reduce(_+_))

  def text : Parser[Token] = """[^{}',.; \t\n]+""".r ^^ (s => Text(s))

  def spacing : Parser[Token] = """[',.; \t]+""".r ^^ (s => Text(s))

  def line : Parser[List[Token]] =
    indent ~ rep1((verbatim | control | tag | text | spacing)) ^^ {case indent ~ toks => indent :: toks} |
    "[ \t]*".r ^^^ Nil

  def input : Parser[List[Token]] =
    repsep(line,"\n") ^^ (_.flatten)
}

abstract class PrintEngine[T] {
  val empty : T
//  def + (x:T,y:T) : T
  def text(txt:String) : T
  def open(tag:Tag) : T
  def close(tag:Tag) : T
  def indent(n:Int) : T
}

abstract class StringPrintEngine extends PrintEngine[String] {
  val empty = ""
  def text(txt:String) = txt
  def indent(n:Int) : String = "\n" + " " * n
}

object HtmlPrintEngine extends StringPrintEngine {
  def open(tag:Tag) = tag match {
    case Tag(name,attrs) =>
      "<" + name +
      (attrs match { case Some(a) => " "+a; case None => "" }) +
      ">"
  }
  def close(tag:Tag) = tag match {
    case Tag(name,attrs) =>
      "</" + name + ">"
  }
}

class Printer(printEngine: PrintEngine[String]) {

  private def printTag(input:List[Token], acc:String, indent:Int, tag:Tag) : (List[Token], String, Int)  = {
    val tag_indent = indent
    def printContents(input:List[Token], acc:String, indent:Int) : (List[Token], String) =
      (tag,input) match {
        case (_:IndentTag, Indent(n)::r) if (n <= tag_indent) =>
          //Console.printf("Closing indent tag %s with indent %d\n", tag.name, indent)
          val indentUnlessInline = {
            if (tag_indent < Int.MaxValue && indent < Int.MaxValue)
              printEngine.indent(tag_indent)
            else printEngine.empty
          }
          (input, acc + indentUnlessInline)
        case (_:BraceTag, ClosingBrace::r) =>
          (r, acc)
        case (_, Nil) =>
          (input, acc + printEngine.indent(tag_indent))
        case _ =>
          //Console.printf("Next item in tag %s with indent %d\n", tag.name, indent)
          val (input2, acc2, indent2) = printElem(input, acc, indent)
          printContents(input2, acc2, indent2)
      }
    //Console.printf("Opening tag %s with indent %d\n", tag.name, indent)
    val (input2, acc2) = printContents(input, acc + printEngine.open(tag), Int.MaxValue)
    (input2, acc2 + printEngine.close(tag), indent)
  }

  private def printElem(input:List[Token], acc:String, indent:Int): (List[Token], String, Int) =
    input match {
      case Indent(n)::r =>
        val indentUnlessFirst = (if (indent >= 0) printEngine.indent(n) else printEngine.empty)
        (r, acc + indentUnlessFirst, n)
      case Text(str)::r => (r, acc + printEngine.text(str), indent)
      case (tag:Tag)::r => printTag(r, acc, indent, tag)
      case ClosingBrace::r => System.err.println("Warning: extra '}'")
        (r, acc + "}", indent)
      case Nil =>
        (Nil, acc, 0)
    }

  private def printInput(input:List[Token], acc:String, indent:Int): String =
    input match {
      case tok::r =>
        val (input2, acc2, indent2) = printElem(input, acc, indent)
        printInput(input2,acc2,indent2)
      case Nil => acc
    }

  def apply(input:List[Token]) : String = printInput(input, printEngine.empty, -1)
}

import scala.util.parsing.input._
import java.io._

val in = StreamReader(new FileReader(args(0)))

val toks = IndentLexer.parse(IndentLexer.input,in).get
val htmlPrinter = new Printer(HtmlPrintEngine)
scala.Console.printf("%s",htmlPrinter(toks))

// (java.io.Reader => scala PagedSeq => scala StreamReader

// scala.io.BufferedSource => ... => CharSequenceReader <: Reader[Char] == IndentLexer.Input)
