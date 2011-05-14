package svoysh.util

import org.specs._
import org.specs.runner._

class StringsSpecTest extends JUnit4(StringsSpec)

object StringsSpecRunner extends ConsoleRunner(StringsSpec)

object StringsSpec extends Specification {

	"isEmpty" should {

		"return true" in {
			List(
				null,
				"",
				" ",
				"\t",
				"\n",
				" \t\n\f\r"
			).foreach(Strings.isEmpty(_) must beTrue)
		}

		"return false" in {
			List(
				"123",
				"abc",
				" a",
				"a ",
				" a ",
				" 123 abc ",
				"\t123\nabc\r"
			).foreach(Strings.isEmpty(_) must beFalse)
		}
	}

	"rtrim" should {

		"trim tailing whitespaces" in {
			List(
				"a",
				"a ",
				"a\t",
				"a\n",
				"a \t\n\f\r"
			).foreach(rtrim(_) mustEqual "a")

			List(
				"",
				" ",
				"\t",
				"\n",
				" \t\n\f\r"
			).foreach(rtrim(_) mustEqual "")

			rtrim(null) must beNull
			rtrim(" a ") mustEqual " a"
			rtrim("\t\n\f\ra\t\n\f\r") mustEqual "\t\n\f\ra"
		}
	}
}