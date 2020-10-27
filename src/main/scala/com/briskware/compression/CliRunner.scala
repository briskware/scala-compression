package com.briskware.compression


import java.nio.charset.StandardCharsets


object CliRunner extends App  {

  implicit val logger: Logger = Logger.getConsoleLogger(Logger.DEBUG)

  val compressor = ShannonFano()

  val textToCompress: String = "ACABADADEAABBAAAEDCACDEAAABCDBBEDCBACAE"

  val bytes: List[Byte] = textToCompress.getBytes(StandardCharsets.US_ASCII).toList

  val cData: CompressedData = compressor.encodeBytes(bytes)

  val decodedBytes = compressor.decodeBits(cData)

  logger.info("Decompressed content follows below:")

  val decompressedString = new java.lang.String(decodedBytes.toArray, "us-ascii") //decodedBytes.map(_.toChar).mkString("[", "", "]")

  logger.info(decompressedString)

  assert(decompressedString == textToCompress, "strings don't match")

}

// EOF
