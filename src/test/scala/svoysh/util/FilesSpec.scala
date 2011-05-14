package svoysh.util

import org.specs._
import org.specs.runner._

import net.liftweb.common._

class FilesSpecTest extends JUnit4(FilesSpec)

object FilesSpecRunner extends ConsoleRunner(FilesSpec)

object FilesSpec extends Specification {

	"getFileName" should {

		"return some name" in {
			List(
				"/uploads/image.jpg",
				"/uploads/image.jpg",
				"/uploads/image.jpg",
				"uploads/image.jpg",
				"/image.jpg",
				"\\uploads\\image.jpg",
				"\\uploads/image.jpg",
				"/uploads\\image.jpg",
				" /image.jpg "
			).foreach(getFileName(_) mustEqual Full("image.jpg"))

			getFileName(" image") mustEqual Full(" image")
			getFileName("/ image") mustEqual Full(" image")
			getFileName("/image.") mustEqual Full("image.")
			getFileName("/image") mustEqual Full("image")
			getFileName("/.jpg") mustEqual Full(".jpg")
			getFileName("/.") mustEqual Full(".")
			getFileName(".") mustEqual Full(".")
		}

		"return no name (Empty)" in {
			List(
				null,
				" ",
				"",
				"/a/",
				"a/",
				"a\\",
				"/a\\",
				"\\a/",
				" \\a/ "
			).foreach(getFileName(_) mustEqual Empty)
		}
	}

	"getFileNameWithoutExt" should {

		"return some name" in {
			List(
				"image.jpg",
				"image.",
				"image",
				"/uploads/image.jpg",
				"/uploads/image.",
				"/uploads/image",
				"uploads/image.jpg",
				"/image.jpg",
				"/uploads\\image.jpg",
				"\\uploads/image.jpg",
				" /image.jpg "
			).foreach(getFileNameWithoutExt(_) mustEqual Full("image"))

			getFileNameWithoutExt(" image.jpg ") mustEqual Full(" image")
			getFileNameWithoutExt(" image. ") mustEqual Full(" image")
			getFileNameWithoutExt(" image ") mustEqual Full(" image")
		}

		"return no name (Empty)" in {
			List(
				null,
				".jpg",
				".",
				" ",
				"",
				"/a/",
				"a/",
				"a\\"
			).foreach(getFileNameWithoutExt(_) mustEqual Empty)
		}
	}

	"getFileExt" should {

		"return some ext" in {
			List(
				"image.jpg ",
				"image.jpg",
				".jpg"
			).foreach(getFileExt(_) mustEqual Full("jpg"))
		}

		"return no ext (Empty)" in {
			List(
				"image.",
				"image",
				".",
				" ",
				"",
				null
			).foreach(getFileExt(_) mustEqual Empty)
		}
	}

	"getFileExtWithDot" should {

		"return some ext" in {
			List(
				"image.jpg ",
				"image.jpg",
				".jpg"
			).foreach(getFileExtWithDot(_) mustEqual Full(".jpg"))
		}

		"return no ext (Empty)" in {
			List(
				"image.",
				"image",
				".",
				" ",
				"",
				null
			).foreach(getFileExtWithDot(_) mustEqual Empty)
		}
	}
}