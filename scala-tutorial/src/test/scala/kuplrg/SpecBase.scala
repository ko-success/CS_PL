package kuplrg

import java.io.{File, PrintWriter}
import java.util.concurrent.TimeoutException
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import scala.Console.*
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.*
import scala.io.Source
import scala.quoted.*
import scala.util.Try

trait SpecBase extends AnyFlatSpec with BeforeAndAfterAll {
  import SpecBase.*

  try Implementation catch
    case err: Throwable =>
      printlnColor(RED)(unexpectedErrorMsg(err))
      throw err

  val DEFAULT_TIME_LIMIT = 5
  val TIME_LIMIT = Try(readFile("time_limit").trim.toInt)
    .getOrElse(DEFAULT_TIME_LIMIT)

  def check[T](
    exprCode: String,
    result: => T,
  ): Unit = newTest(exprCode) {
    result
    None
  }

  inline def test[T](inline expr: T): Unit =
    ${ testImpl('expr, 'check) }

  def check[T](
    exprCode: String,
    answerCode: String,
    result: => T,
    expected: => T,
  ): Unit = newTest(exprCode, s"be $answerCode") {
    if (result != expected) Some(s"$result != $expected")
    else None
  }

  inline def test[T](inline expr: T, inline answer: T): Unit =
    ${ testImpl('expr, 'answer, 'check) }

  def checkExc[T](
    exprCode: String,
    result: => T,
    errMsg: => String,
  ): Unit = newTest(exprCode, s"throw an error with a message \"$errMsg\"") {
    try
      Some(s"""It must throw an error but returns $result without errors""")
    catch
      case err: PLError =>
        val msg = err.msg
        if (!err.msg.contains(errMsg))
          Some(s"""Error message should contain "$errMsg" but got "$msg"""")
        else None
  }

  inline def testExc[T](inline expr: T, inline errMsg: String): Unit =
    ${ testExcImpl('expr, 'errMsg, 'checkExc) }

  def afterTest: Unit = ()

  private def newTest[T](
    exprCode: String,
    cond: String
  )(result: => Option[String]): Unit = newTest(exprCode, Some(cond))(result)
  private def newTest[T](
    exprCode: String,
    cond: Option[String] = None
  )(result: => Option[String]): Unit =
    exprCode should cond.getOrElse("be terminated normally") in {
    info(exprCode + cond.fold("")(cond => s" should $cond"))
    testCount += 1
    val msg = try
      Await.result(Future(Try(result)), TIME_LIMIT.second).get
    catch
      case err: NotImplementedError => failed(notImpl(err))
      case _: TimeoutException => failed("Timeout")
      case err: Throwable => failed(err)
    msg.fold(passed)(failed(_))
  }

  private def info(msg: String): Unit =
    printColor(CYAN)(s"[test] ")
    print(msg)

  private def failed(err: Throwable): Nothing =
    val msg = err match
      case err: PLError => err.msg
      case _ => unexpectedErrorMsg(err)
    failed(s"[ERROR] $msg")
  private def failed(msg: String): Nothing =
    printlnColor(RED)(" - FAIL")
    printlnColor(RED)(s"       $msg")
    failCount += 1
    fail(msg)
  private def notImpl(err: NotImplementedError): String =
    val elem = err.getStackTrace.apply(1)
    val line = elem.getLineNumber
    val methodName = elem.getMethodName
    s"Please implement `${norm(methodName)}` (Implementaion.scala:$line)."

  private def unexpectedErrorMsg(err: Throwable): String = (
    s"Unexpected Error - $err\n" +:
    err.getStackTrace.take(10).toVector
    :+ "..."
  ).mkString("\n       ")

  private def passed: Unit =
    printlnColor(GREEN)(" - PASS")
    passCount += 1

  private var testCount = 0
  private var failCount = 0
  private var passCount = 0
  override def afterAll(): Unit =
    val score = passCount * 100 / (passCount + failCount)
    println("----------------------------------------")
    println(s"[SCORE] $score ($passCount / ${passCount + failCount})")
    println("----------------------------------------")
    dumpFile(score, "score")
    try afterTest catch
      case err: Throwable => printlnColor(RED)("[Error] " + (err match
        case err: NotImplementedError => notImpl(err)
        case _ => unexpectedErrorMsg(err)
      ))

  def mkdir(name: String): Unit = File(name).mkdirs

  def getPrintWriter(filename: String): PrintWriter =
    val file = File(filename)
    val parent = file.getParent
    if (parent != null) mkdir(parent)
    PrintWriter(file)

  def readFile(filename: String): String =
    val source = Source.fromFile(filename, "utf8")
    val str = source.mkString
    source.close
    str

  def dumpFile(data: Any, filename: String): Unit =
    val nf = getPrintWriter(filename)
    nf.print(data)
    nf.close()
}

private object SpecBase:
  def testImpl[T](
    expr: Expr[T],
    check: Expr[(String, => T) => Unit],
  )(using Type[T], Quotes): Expr[Unit] =
    val exprCode = normCode(expr)
    '{ $check($exprCode, $expr) }

  def testImpl[T](
    expr: Expr[T],
    answer: Expr[T],
    check: Expr[(String, String, => T, => T) => Unit],
  )(using Type[T], Quotes): Expr[Unit] =
    val exprCode = normCode(expr)
    val answerCode = normCode(answer)
    '{ $check($exprCode, $answerCode, $expr, $answer) }

  def testExcImpl[T](
    expr: Expr[T],
    errMsg: Expr[String],
    checkExc: Expr[(String, => T, => String) => Unit],
  )(using Type[T], Quotes): Expr[Unit] = '{
    $checkExc(${ normCode(expr) }, $expr, $errMsg)
  }

  val len = 70

  def normCode[T](expr: Expr[T])(using Quotes): Expr[String] =
    Expr(norm(expr.show))

  def norm(expr: Any): String =
    val str = expr.toString
      .replaceAll("this.", "")
      .replaceAll("kuplrg.", "")
      .replaceAll("scala\\.", "")
      .replaceAll("Spec\\.", "")
      .replaceAll("Implementation\\.", "")
      .replaceAll("\\.apply", "")
      .replaceAll("\\w+\\$package\\.", "")
      .replaceAll("\\$\\w*", "")
    if (str.length <= len) str
    else s"${str.substring(0, len)} ..."

  /** show colored message */
  def setColor(color: String): Any => String =
    if (color == "") x => x.toString else x => color + x.toString + RESET
  def printColor(color: String): Any => Unit = x => print(setColor(color)(x))
  def printlnColor(color: String): Any => Unit = x =>
    println(setColor(color)(x))
