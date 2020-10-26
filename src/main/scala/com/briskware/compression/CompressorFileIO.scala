package com.briskware.compression

import java.io.{File, FileOutputStream}

import scala.util.Try

object CompressorFileIO {
  def fromFile(file: File): CompressedData = {
    ???
  }
}

class CompressorFileIO(data: CompressedData) {

  def toFile(file: File): Try[Boolean] = {
    for {
      os <- Try(new FileOutputStream(file))
      h <- writeHeader(os)
      b <- writeBody(os)
      c <- writeChecksum(os)
      e <- Try(os.close())
    } yield {
      true
    }
  }

  protected def writeHeader(os: FileOutputStream): Try[Boolean] = {
    Try {
      true
    }
  }

  protected def writeBody(os: FileOutputStream): Try[Boolean] = ???

  protected def writeChecksum(os: FileOutputStream): Try[Boolean] = ???

  private def writeBits(os: FileOutputStream, bs: Bits): Try[Int] = {
    ???
  }

  def getChecksum(): Try[Bits] = ???

  def validateChecksum(): Try[Boolean] = ???
}

//EOF
