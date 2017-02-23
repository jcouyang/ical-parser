package us.oyanglul.icalparser
import shapeless._, syntax.singleton._

import scala.collection.immutable.{ :: => Cons }
// import java.util.Date
import scala.util.{ Try, Success, Failure }

object ICalParserExample extends App {

  val input = """BEGIN:VCALENDAR
PRODID;X-RICAL-TZSOURCE=TZINFO:-//com.denhaven2/NONSGML ri_cal gem//EN
CALSCALE:GREGORIAN
VERSION:2.0
X-WR-CALNAME:On Call Schedule
BEGIN:VEVENT
DTEND;VALUE=DATE-TIME:20170123T080000Z
DTSTART;VALUE=DATE-TIME:20170122T220000Z
ATTENDEE:oyanglulu@gmail.com
UID:Q3DNWZL0W9JV04
URL:https://blog.oyanglul.us/schedules#PFWMMZA
SUMMARY:On Call - Jichao Ouyang - Lego Master
END:VEVENT
BEGIN:VEVENT
DTEND;VALUE=DATE-TIME:20170123T080000Z
DTSTART;VALUE=DATE-TIME:20170122T220000Z
ATTENDEE:oyanglulu@gmail.com
UID:Q3DNWZL0W9JV05
URL:https://blog.oyanglul.us/schedules#PFWMMZA
SUMMARY:On Call - Jichao Ouyang - Lego Master
END:VEVENT
END:VCALENDAR
"""

  import ICalParser._

  println(ICalParser[List[Event]].from(input))

}

class ICalException(s: String) extends RuntimeException

case class Event(end: String, start: String, attendee: String, id: String, url: String, summary: String)

trait ICalParser[T] {
  def from(s: String): Try[T]
}

object ICalParser {

  def apply[T](implicit st: Lazy[ICalParser[T]]): ICalParser[T] = st.value

  def fail(s: String) = {
    println(s)
    Failure(new ICalException(s))
  }

  implicit def stringICalParser: ICalParser[String] = new ICalParser[String] {
    def from(s: String): Try[String] = Success(s)
  }

  def listICalLinesParser[A](l: List[String])(implicit ec: ICalParser[A]): Try[List[A]] = l match {
    case Nil => Success(Nil)
    case Cons(s, ss) => for {
      x <- ec.from(s)
      xs <- listICalLinesParser(ss)(ec)
    } yield Cons(x, xs)
  }

  def splitEvents(s: String): Array[String] =
    """(END:VEVENT)|(BEGIN:VEVENT)""".r.split(s).tail.reverse.tail.filterNot(x => x.isEmpty || x == "\n")

  implicit def listICalParser[A](implicit ec: ICalParser[A]): ICalParser[List[A]] = new ICalParser[List[A]] {
    def from(s: String): Try[List[A]] = listICalLinesParser(splitEvents(s).toList)(ec)
  }
  implicit def deriveHNil: ICalParser[HNil] =
    new ICalParser[HNil] {
      def from(s: String): Try[HNil] = s.trim match {
        case "\n" => Success(HNil)
        case "" => Success(HNil)
        case s => fail("Cannot convert '" ++ s ++ "' to HNil")
      }
    }
  implicit def genericEvent[V, T <: HList](implicit stringParser: ICalParser[V], genericParser: Lazy[ICalParser[T]]): ICalParser[V :: T] =
    new ICalParser[V :: T] {
      def from(s: String): Try[V :: T] = s.span(_ != ':')._2.span(_ != '\n') match {
        case (head, tail) => for {
          front <- stringParser.from(head.tail)
          back <- genericParser.value.from(tail)
        } yield front :: back
        case _ => fail("Cannot convert '" ++ s ++ "' to HList")
      }
    }

  implicit def deriveCaseClass[T, R](implicit gen: Generic.Aux[T, R], conv: ICalParser[R]): ICalParser[T] = new ICalParser[T] {
    def from(s: String): Try[T] = conv.from(s).map(gen.from)
  }
}
