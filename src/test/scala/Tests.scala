import util.{Pixel, Util}

class Tests extends munit.FunSuite {

  val DEBUG = true

  def pickColor(i: Integer) : Pixel = {
    if (i == 0) Pixel(255, 0, 0)
    else if (i == 1) Pixel(0, 0, 255)
    else if (i == 2) Pixel(0, 255, 0)
    else if (i == 3) Pixel(255, 255, 255)
    else Pixel(0, 0, 0)
  }

  // vertical
  test("Vertical Small (2p)") {
    val black = Solution.fromStringPPM(Util.readEntire("input/black.ppm").toList)
    val white = Solution.fromStringPPM(Util.readEntire("input/white.ppm").toList)
    val result = Solution.toStringPPM(Solution.verticalConcat(black, white))
    if (DEBUG) Util.print(result.mkString, "vertical_small.ppm")
    val verification = Util.verifyResult(result.mkString, "vertical_small.ppm")
    assert(verification._1, verification._2)
  }

  test("Vertical Big (8p)") {
    val big1 = Solution.fromStringPPM(Util.readEntire("input/v_big_1.ppm").toList)
    val big2 = Solution.fromStringPPM(Util.readEntire("input/v_big_2.ppm").toList)
    val result = Solution.toStringPPM(Solution.verticalConcat(big1, big2))
    if (DEBUG) Util.print(result.mkString, "vertical_big.ppm")
    val verification = Util.verifyResult(result.mkString, "vertical_big.ppm")
    assert(verification._1, verification._2)
  }

  // horizontal
  test("Horizontal Small (2p)") {
    val black = Solution.fromStringPPM(Util.readEntire("input/black.ppm").toList)
    val white = Solution.fromStringPPM(Util.readEntire("input/white.ppm").toList)
    val result = Solution.toStringPPM(Solution.horizontalConcat(black, white))
    if (DEBUG) Util.print(result.mkString, "horizontal_small.ppm")
    val verification = Util.verifyResult(result.mkString, "horizontal_small.ppm")
    assert(verification._1, verification._2)
  }

  test("Horizontal Big (3p)") {
    val big1 = Solution.fromStringPPM(Util.readEntire("input/h_big_1.ppm").toList)
    val big2 = Solution.fromStringPPM(Util.readEntire("input/h_big_2.ppm").toList)
    val result = Solution.toStringPPM(Solution.horizontalConcat(big1, big2))
    if (DEBUG) Util.print(result.mkString, "horizontal_big.ppm")
    val verification = Util.verifyResult(result.mkString, "horizontal_big.ppm")
    assert(verification._1, verification._2)
  }

  // rotate
  test("Trigonometry-wise rotation 90 (1p)") {
    val input = Solution.fromStringPPM(Util.readEntire("input/rotation.ppm").toList)
    val result = Solution.toStringPPM(Solution.rotate(input, 90))
    if (DEBUG) Util.print(result.mkString, "trigonometry_rotation_1.ppm")
    val verification = Util.verifyResult(result.mkString, "trigonometry_rotation_1.ppm")
    assert(verification._1, verification._2)
  }

  test("Trigonometry-wise rotation 180 (1p)") {
    val input = Solution.fromStringPPM(Util.readEntire("input/rotation.ppm").toList)
    val result = Solution.toStringPPM(Solution.rotate(input, 180))
    if (DEBUG) Util.print(result.mkString, "trigonometry_rotation_2.ppm")
    val verification = Util.verifyResult(result.mkString, "trigonometry_rotation_2.ppm")
    assert(verification._1, verification._2)
  }

  test("Trigonometry-wise rotation 270 (1p)") {
    val input = Solution.fromStringPPM(Util.readEntire("input/rotation.ppm").toList)
    val result = Solution.toStringPPM(Solution.rotate(input, 270))
    if (DEBUG) Util.print(result.mkString, "trigonometry_rotation_3.ppm")
    val verification = Util.verifyResult(result.mkString, "trigonometry_rotation_3.ppm")
    assert(verification._1, verification._2)
  }

  test("Trigonometry-wise rotation 360 (2p)") {
    val input = Solution.fromStringPPM(Util.readEntire("input/rotation.ppm").toList)
    val result = Solution.toStringPPM(Solution.rotate(input, 360))
    if (DEBUG) Util.print(result.mkString, "trigonometry_rotation_4.ppm")
    val verification = Util.verifyResult(result.mkString, "trigonometry_rotation_4.ppm")
    assert(verification._1, verification._2)
  }

  test("Edge Detection Teapot (20p)") {
    val input = Solution.fromStringPPM(Util.readEntire("input/Teapot.ppm").toList)
    val result = Solution.toStringPPM(Solution.edgeDetection(input, 0.3))
    if (DEBUG) Util.print(result.mkString, "teapot.ppm")
    val verification = Util.verifyResult(result.mkString, "teapot.ppm")
    assert(verification._1, verification._2)
  }

  test("Edge Detection Lenna (20p)") {
    val input = Solution.fromStringPPM(Util.readEntire("input/Lenna.ppm").toList)
    val result = Solution.toStringPPM(Solution.edgeDetection(input, 0.3))
    if (DEBUG) Util.print(result.mkString, "lenna.ppm")
    val verification = Util.verifyResult(result.mkString, "lenna.ppm")
    assert(verification._1, verification._2)
  }

  // modulo pascal
  test("Modulo Pascal 2 (10p)"){
    val result = Solution.toStringPPM(Solution.moduloPascal(2, pickColor, 16))
    if (DEBUG) Util.print(result.mkString, "modulo_pascal_2.ppm")
    val verification = Util.verifyResult(result.mkString, "modulo_pascal_2.ppm")
    assert(verification._1, verification._2)
  }

  test("Modulo Pascal 3 (15p)"){
    val result = Solution.toStringPPM(Solution.moduloPascal(3, pickColor, 27))
    if (DEBUG) Util.print(result.mkString, "modulo_pascal_3.ppm")
    val verification = Util.verifyResult(result.mkString, "modulo_pascal_3.ppm")
    assert(verification._1, verification._2)
    val result2 = Solution.toStringPPM(Solution.moduloPascal(3, pickColor, 81))
    if (DEBUG) Util.print(result2.mkString, "modulo_pascal_3_big.ppm")
    val verification2 = Util.verifyResult(result2.mkString, "modulo_pascal_3_big.ppm")
    assert(verification2._1, verification2._2)
  }

  test("Modulo Pascal 5 (15p)"){
    val result = Solution.toStringPPM(Solution.moduloPascal(5, pickColor, 125))
    if (DEBUG) Util.print(result.mkString, "modulo_pascal_5.ppm")
    val verification = Util.verifyResult(result.mkString, "modulo_pascal_5.ppm")
    assert(verification._1, verification._2)
  }
}
