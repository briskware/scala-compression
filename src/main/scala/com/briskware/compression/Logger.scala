package com.briskware.compression

case class LoggerLevel(level: Int, str: String)

trait Logger {

  import Logger._

  val level: LoggerLevel = INFO

  /** logs argument
    */
  def _log(_msg: String, _level: LoggerLevel): Unit

  def trace(msg: String): Unit = _log(msg, TRACE)
  def debug(msg: String): Unit = _log(msg, DEBUG)
  def info(msg: String): Unit = _log(msg, INFO)
  def warn(msg: String): Unit = _log(msg, WARN)
  def error(msg: String): Unit = _log(msg, ERROR)

}

class ConsoleLogger extends Logger {

  /** logs argument
    */
  override def _log(_msg: String, _level: LoggerLevel): Unit = if (level.level <= _level.level) _level match {
    case LoggerLevel(level, str) if level < 3 => Console.out.println(s"$str: ${_msg}")
    case LoggerLevel(level, str) => Console.err.println(s"$str: ${_msg}")
  }
}

object Logger {

  val TRACE = LoggerLevel(0, "TRACE")
  val DEBUG = LoggerLevel(1, "DEBUG")
  val INFO = LoggerLevel(2, "INFO")
  val WARN = LoggerLevel(3, "WARN")
  val ERROR = LoggerLevel(4, "ERROR")

  def getConsoleLogger(_level: LoggerLevel = INFO): Logger = new ConsoleLogger {
    override val level: LoggerLevel = _level
  }
}

//EOF
