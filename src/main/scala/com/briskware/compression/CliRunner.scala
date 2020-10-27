package com.briskware.compression


import java.io.{StringReader, StringWriter}
import java.nio.charset.StandardCharsets

import scala.util.{Success, Try}


object CliRunner extends App  {

  implicit val logger: Logger = Logger.getConsoleLogger(Logger.TRACE)

  val compressor = ShannonFano()

  val textToCompress: String = "ACABADADEAABBAAAEDCACDEAAABCDBBEDCBACAE"

  val bytes: List[Byte] = textToCompress.getBytes(StandardCharsets.US_ASCII).toList

  val cData: CompressedData = compressor.encodeBytes(bytes)

  val decodedBytes = compressor.decodeBits(cData)

  logger.info("Decompressed content follows below:")

  val decompressedString = new java.lang.String(decodedBytes.toArray, "us-ascii") //decodedBytes.map(_.toChar).mkString("[", "", "]")

  logger.info(decompressedString)

  assert(decompressedString == textToCompress, "strings don't match")

  val sWriter = new StringWriter()

  logger.debug(s"Original tree: ${cData.tree}")

  val io = new CompressorFileIO(cData)
  io.writeTree(sWriter)

  val serialisedTree = sWriter.getBuffer.toString

  logger.info(s"Serialised tree: ${serialisedTree}")

  //0 0 165 166 0 168 0 167 169
  //0 0 141      142      0 144      0 143      145
  //0 0 11000001 11000010 0 11000100 0 11000011 11000101

  val sReader = new StringReader(serialisedTree)

  val Success(tree) = CompressorFileIO.readTree(sReader)

  logger.debug(s"Reconstructed tree: ${tree}")

  val fWriter = new StringWriter()
  io.writeAll(fWriter)

  logger.info(s"All (lenght=${fWriter.getBuffer.toString.length}): ${fWriter.getBuffer.toString}")
}

// EOF
