package svoysh.util

import org.specs._
import org.specs.runner._

class VarBoxSpecTest extends JUnit4(VarBoxSpec)

object VarBoxSpecRunner extends ConsoleRunner(VarBoxSpec)

object VarBoxSpec extends Specification {

	/**
	 * Hack to make Specs not ignore an empty body of matched cases.
	 * Usage:
	 * <pre>
	 *     "abc" match {
	 *         case null => fail()
	 *         case "abc" => matched
	 *         case _ => fail()
	 *     }
	 * </pre>
	 */
	def matched = 1 must notBeNull

	"VarBox value" should {
		"be initialized lazily" in {
			var lazyCalled = false
			val lazyVarBox = VarBox(() => {
				lazyCalled = true
				"Lazy function"
			})
			lazyCalled must beFalse
			lazyVarBox.is mustEqual "Lazy function"
			lazyCalled must beTrue
		}

		val defVarBoxV = VarBox("Init value", "Default value")
		"be matched to initial but not default" in {
			defVarBoxV.is mustEqual "Init value"
		}
		"be matched default after cleanup" in {
			defVarBoxV.clear
			defVarBoxV.is mustEqual "Default value"
		}

		var defCalled = false
		val defVarBoxFn = VarBox("Init value", () => {
			defCalled = true
			"Default function"
		})
		"be matched to initial but not default (function)" in {
			defCalled must beFalse
			defVarBoxFn.is mustEqual "Init value"
			defCalled must beFalse
		}
		"be matched default (function) after cleanup" in {
			defVarBoxFn.clear
			defCalled must beFalse
			defVarBoxFn.is mustEqual "Default function"
			defCalled must beTrue
		}
	}

	"VarBoxes" should {
		"be equal" in {
			VarBox() mustEqual VarBox()
			VarBox(2) mustEqual VarBox(2)

			val boxB = VarBox("B")
			boxB mustEqual VarBox("B")

			val lazyB = VarBox(() => "B")
			lazyB mustEqual boxB

			val boxB_defC = VarBox("B", "C")
			boxB_defC mustEqual boxB
			boxB_defC.clear
			boxB_defC mustEqual VarBox("C")

			val emptyVarBox = VarBox()
			emptyVarBox mustEqual emptyVarBox

			// Maybe they should not be equal?
			VarBox[Int]() must_== VarBox[String]()
		}
	}

	"VarBoxes" should {
		"be not equal" in {
			VarBox() must_!= VarBox(1)
			VarBox(2) must_!= VarBox(3)

			val boxB = VarBox("B")
			boxB must_!= VarBox("C")
			boxB.clear
			boxB must_!= VarBox("B")

			val nullBoxInt: VarBox[Int] = null
			boxB must_!= nullBoxInt
		}
	}

	"VarBoxes" should {
		"change its value on apply" in {
			val box = VarBox(1)
			box mustEqual VarBox(1)
			box(2)
			box mustEqual VarBox(2)
			box.clear
			box mustEqual VarBox[Int]()
			box(3)
			box mustEqual VarBox(3)
		}
	}

	"VarBox variable" should {
		"match VarBox(_)" in {
			val matchVarBox: VarBox[Int] = VarBox(2)
			matchVarBox match {
				case VarBox(1) => fail()
				case VarBox(2) => matched
				case VarBox(x) => fail()
				case _ => fail()
			}
		}

		"match VarBox(_) with lazy value" in {
			val matchLazyVarBox: VarBox[Int] = VarBox(() => 2)
			matchLazyVarBox match {
				case VarBox(1) => fail()
				case VarBox(2) => matched
				case VarBox(x) => fail()
				case _ => fail()
			}
		}

		val matchDefVarBox: VarBox[Int] = VarBox(1, () => 2)
		"match VarBox(_) with default value" in {
			matchDefVarBox match {
				case VarBox(1) => matched
				case VarBox(2) => fail()
				case VarBox(x) => fail()
				case _ => fail()
			}
		}
		"match VarBox(_) with default value (function)" in {
			matchDefVarBox.clear
			matchDefVarBox match {
				case VarBox(1) => fail()
				case VarBox(2) => matched
				case VarBox(x) => fail()
				case _ => fail()
			}
		}

		val matchUnderscoreVarBox: VarBox[Int] = VarBox(2)
		"match VarBox(_) (underscore)" in {
			matchUnderscoreVarBox match {
				case VarBox(1) => fail()
				case VarBox(_) => matched
				case _ => fail()
			}
		}
		"match _ (underscore)" in {
			matchUnderscoreVarBox match {
				case _ => matched
			}
		}
	}

	"VarBox with no value" should {
		"match VarBox()" in {
			val matchEmptyVarBox: VarBox[Int] = VarBox[Int]()
			matchEmptyVarBox match {
				case VarBox(1) => fail()
				case VarBox(2) => fail()
				case VarBox(x) => fail()
				case VarBox() => matched
				case _ => fail()
			}
		}
	}

	"Cleaned VarBox" should {
		"match VarBox()" in {
			val matchCleanedVarBoxAsEmpty: VarBox[Int] = VarBox[Int](2)
			matchCleanedVarBoxAsEmpty.clear
			matchCleanedVarBoxAsEmpty match {
				case VarBox(1) => fail()
				case VarBox(2) => fail()
				case VarBox(x) => fail()
				case VarBox() => matched
				case _ => fail()
			}
		}
	}
}