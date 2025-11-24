import util.Pixel
import util.Util.toGrayScale
import scala.annotation.tailrec

object Solution {

  type Image = List[List[Pixel]]
  type GrayscaleImage = List[List[Double]]

  def parse(image: List[Char])(delim: Char): (List[Char], List[Char]) = {
    image match {
      case Nil => (Nil, Nil)
      case x :: xs =>
        if (x == delim) (Nil, xs)
        else {
          val (partial, rest) = parse(xs)(delim)
          (x :: partial, rest)
        }
    }
  }

  def toInt(number: List[Char]): Integer = {
    number.foldLeft(0)((acc, x) => acc * 10 + (x - '0'))
  }

  def getPixels(image: List[Char], length: Int): (List[Pixel], List[Char]) = {
    if (length == 0) (Nil, image)
    else {
      val (r, rest1) = parse(image)(' ')
      val (g, rest2) = parse(rest1)(' ')
      val (b, rest3) = parse(rest2)('\n')
      val (pixelList, rest4) = getPixels(rest3, length - 1)
      (Pixel(toInt(r), toInt(g), toInt(b)) :: pixelList, rest4)
    }
  }

  def getListsOfPixels(image: List[Char], length: Int, height: Int): Image = {
    if (height == 0) Nil
    else {
      val (pixels, rest) = getPixels(image, length)
      pixels :: getListsOfPixels(rest, length, height - 1)
    }
  }

  def fromStringPPM(image: List[Char]): Image = {
    val (_: List[Char], rest1) = parse(image)('\n')
    val (length, rest2) = parse(rest1)(' ')
    val (height, rest3) = parse(rest2)('\n')
    val (_, rest4) = parse(rest3)('\n')

    getListsOfPixels(rest4, toInt(length), toInt(height))
  }

  def pixelToCharList(pixel: Pixel): List[Char] = {
    pixel.red.toString.toList ++ " " ++ pixel.green.toString.toList ++ " " ++ pixel.blue.toString.toList
  }

  def toStringPPM(image: Image): List[Char] = {
    val length = getImageLength(image)
    val height = getImageHeight(image)
    val addedPoints = image.foldRight(Nil: List[Char])(
      (line, acc) => line.foldRight(Nil: List[Char])(
        (p, acc2) => pixelToCharList(p) ++ (if (acc2 != Nil) List('\n') ++ acc2 else acc2)) ++ (if (acc != Nil) List('\n') ++ acc else acc))
    val addedMaxVal = 255.toString.toList ++ "\n" ++ addedPoints ++ "\n"
    val addedDims = length.toString.toList ++ " " ++ height.toString.toList ++ "\n" ++ addedMaxVal
    val finalRes = "P3".toList ++ "\n" ++ addedDims
    finalRes
  }

  def verticalConcat(image1: Image, image2: Image): Image = {
    image1.foldRight(image2)((x, acc) => x :: acc)
  }

  def horizontalConcat(image1: Image, image2: Image): Image = {
    (image1, image2) match {
      case (Nil, Nil) => Nil
      case (x :: xs, y :: ys) =>
        x.foldRight(y)((z, acc) => z :: acc) :: horizontalConcat(xs, ys)
    }
  }

  def getImageLength[A](image: List[List[A]]): Int = {
    def aux_getImageLength(line: List[A]): Int = {
      line match {
        case Nil => 0
        case _ :: xs => 1 + aux_getImageLength(xs)
      }
    }
    aux_getImageLength(image.head)
  }

  def getImageHeight[A](image: List[List[A]]): Int = {
    image match {
      case Nil => 0
      case _ :: xs => 1 + getImageHeight(xs)
    }
  }

  def buildEmpty[A](height: Int): List[List[A]] = {
    if (height == 0) Nil
    else List() :: buildEmpty(height - 1)
  }

  def auxRotate(image: Image, line: List[Pixel]): Image = {
    (image, line) match {
      case (Nil, Nil) => Nil
      case (x :: xs, y :: ys) => (y :: x) :: auxRotate(xs, ys)
    }
  }

  @tailrec
  def doRotations(image: Image, degrees: Integer): Image = {
    if (degrees == 0) image
    else {
      val height = getImageLength(image)
      val emptyImage = buildEmpty[Pixel](height)
      val rotated = image.foldRight(emptyImage)((line, acc) => auxRotate(acc, line.foldLeft(Nil: List[Pixel])((acc2, x) => x :: acc2)))
      doRotations(rotated, degrees - 90)
    }
  }

  def rotate(image: Image, degrees: Integer): Image = {
    doRotations(image, degrees)
  }

  val gaussianBlurKernel: GrayscaleImage = List[List[Double]](
    List( 1, 4, 7, 4, 1),
    List( 4,16,26,16, 4),
    List( 7,26,41,26, 7),
    List( 4,16,26,16, 4),
    List( 1, 4, 7, 4, 1)
  ).map(_.map(_ / 273))

  val Gx : GrayscaleImage = List(
    List(-1, 0, 1),
    List(-2, 0, 2),
    List(-1, 0, 1)
  )

  val Gy : GrayscaleImage = List(
    List( 1, 2, 1),
    List( 0, 0, 0),
    List(-1,-2,-1)
  )

  def toGrayScaleImage(image: Image): GrayscaleImage = {
    image.map(line => line.map(pixel => toGrayScale(pixel)))
  }

  def sumMatrix(image1: GrayscaleImage, image2: GrayscaleImage, f: (Double, Double) => Double): GrayscaleImage = {
    (image1, image2) match {
      case (Nil, Nil) => Nil
      case (x :: xs, y :: ys) => x.zip(y).map(pair => f(pair._1, pair._2)) :: sumMatrix(xs, ys, f)
    }
  }

  def edgeDetection(image: Image, threshold : Double): Image = {
    val grayImage = toGrayScaleImage(image)
    val gauss = applyConvolution(grayImage, gaussianBlurKernel)
    val Mx = applyConvolution(gauss, Gx)
    val My = applyConvolution(gauss, Gy)
    val sumOfMs = sumMatrix(Mx, My, (x, y) => x.abs + y.abs)
    sumOfMs.map(line => line.map(el => if (el <= threshold) Pixel(0, 0, 0) else Pixel(255, 255, 255)))
  }

  def convolution(l1: List[List[Double]], l2: List[List[Double]]): Double = {
    (l1, l2) match {
      case (Nil, Nil) => 0
      case (x :: xs, y :: ys) => x.zip(y).map(
        pair => pair._1 * pair._2).foldRight(0.0)(_ + _) + convolution(xs, ys)
    }
  }

  def applyConvolution(image: GrayscaleImage, kernel: GrayscaleImage) : GrayscaleImage = {
    val length = getImageLength(image)
    val height = getImageHeight(image)
    val kernelLength = getImageLength(kernel)
    val kernelHeight = getImageHeight(kernel)
    val result = (0 until height - (kernelHeight - 1)).foldRight(Nil: List[List[Double]])(
      (i, acc) => (0 until length - (kernelLength - 1)).toList.map(
        j => convolution(image.drop(i).take(kernelHeight).map(line => line.drop(j).take(kernelLength)), kernel)).
          foldRight(Nil: List[Double])(
            (x, acc2) => x :: acc2) :: acc)
    result
  }

  def getPascal(levels: Integer)(modulo: Integer): List[List[Integer]] = {

    def getNextRow(prevRow: List[Integer]): List[Integer] = {
      prevRow match {
        case Nil => Nil
        case _ :: Nil => 1 :: Nil
        case x :: xs => xs match {
          case y :: Nil => (x % modulo + y % modulo) % modulo :: 1.asInstanceOf[Integer] :: Nil
          case y :: _ => (x % modulo + y % modulo) % modulo :: getNextRow(xs)
        }
      }
    }

    @tailrec
    def auxGetPascal(levels: Integer, acc: List[List[Integer]]): List[List[Integer]] = {
      if (levels == 0) acc
      else acc match {
        case Nil => auxGetPascal(levels - 1, List(1.asInstanceOf[Integer]) ++ getNextRow(Nil) :: acc)
        case x :: _ => auxGetPascal(levels - 1, List(1.asInstanceOf[Integer]) ++ getNextRow(x) :: acc)
      }
    }
    auxGetPascal(levels, Nil).foldLeft(Nil: List[List[Integer]])((acc, line) => line :: acc)
  }

  def moduloPascal(m: Integer, funct: Integer => Pixel, size: Integer): Image = {
    val pascal = getPascal(size)(m)
    val matrix = pascal.map(line => line ++ (0 until size - line.length).map(_ => 4.asInstanceOf[Integer]))
    matrix.map(line => line.map(el => funct(el)))
  }
}