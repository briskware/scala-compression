package com.briskware.compression


import java.nio.charset.StandardCharsets


object CliRunner extends App
  with ShannonFano with ConsoleLogger {

  val _textToCompress: String = "Hello"
  val textToCompress: String =
    s"""|// Traditional Struct
        |struct Color {
        |    red: u8,
        |    green: u8,
        |    blue: u8
        |}
        |
        |// Tuple Struct
        |struct TColor(u8, u8, u8);
        |
        |struct Person {
        |    first_name: String,
        |    last_name: String
        |}
        |
        |impl Person {
        |    // Constructor
        |    fn new(first: &str, last: &str) -> Person {
        |        Person {
        |            first_name: first.to_string(),
        |            last_name: last.to_string()
        |        }
        |    }
        |
        |    fn full_name(&self) -> String {
        |        format!("{} {}", self.first_name, self.last_name)
        |    }
        |
        |    fn set_last_name(&mut self, last: &str) {
        |        self.last_name = last.to_string();
        |    }
        |
        |    fn to_tuple(&self) -> (&str, &str) {
        |        (self.first_name.as_str(), self.last_name.as_str())
        |    }
        |}
        |
        |
        |pub fn run() {
        |    let mut color = Color {
        |        red: 255, green: 0, blue: 0
        |    };
        |
        |    color.blue = 128;
        |    log!("Color: {} {} {}", color.red, color.green, color.blue);
        |
        |    let mut tcolor = TColor(255, 0, 0);
        |
        |    tcolor.2 = 128;
        |    log!("TColor: {} {} {}", tcolor.0, tcolor.1, tcolor.2);
        |
        |    let s = Person::new("Stefan", "Szaniszlo");
        |    log!("Person: {} {}", s.first_name, s.last_name);
        |    log!("Person: {}", s.full_name());
        |
        |    let mut p = Person::new("Theresa", "May");
        |    log!("Person: {}", p.full_name());
        |    p.set_last_name("Johnson");
        |    log!("Person: {}", p.full_name());
        |    log!("Person: {:?}", p.to_tuple())
        |
        |}
  """.stripMargin

  val bytes: List[Byte] = textToCompress.getBytes(StandardCharsets.US_ASCII).toList

  val cData: CompressedData = encodeBytes(bytes)

  val decodedBytes = decodeBits(cData)

  log("Decompressed content follows below:")

  val decompressedString = new java.lang.String(decodedBytes.toArray, "us-ascii") //decodedBytes.map(_.toChar).mkString("[", "", "]")

  log(decompressedString)

  assert(decompressedString == textToCompress, "strings don't match")

}

// EOF
