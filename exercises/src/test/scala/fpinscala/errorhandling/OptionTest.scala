package fpinscala.errorhandling

import org.scalatest.prop.PropertyChecks
import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FunSpec, FeatureSpec, Matchers}
import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter



/**
 * Created by michal on 2/15/15.
 */

class OptionTest extends FunSpec with Matchers with PropertyChecks {

  describe("The Option Trait") {
    describe("The map function") {

      val f = (x: Int) => 2 * x
      val dataset =
        Table(
          ("input", "f", "output"),
          (None, f, None),
          (Some(2), f, Some(4))
        )

      it("should map option") {
        forAll (dataset) { (input: Option[Int], f:(Int)=>Int, output:Option[Int]) =>
          input map f shouldEqual output
        }
      }
    }

    describe("The getOrElse function") {

      val dataset =
        Table(
          ("input", "default", "output"),
          (None,    3,          3      ),
          (Some(2), 3,          2      )
        )

      it("should return Some value for not None and default value for None") {
        forAll (dataset) { (input: Option[Int], default: Int, output: Int) =>
          input getOrElse  default shouldEqual output
        }
      }
    }


    describe("The flatMap function") {

      val f = (x: Int) => Some(2 * x)
      val dataset =
        Table(
          ("input", "f", "output" ),
          (None,      f,  None    ),
          (Some(2),   f,  Some(4) )
        )

      it("should map option") {
        forAll (dataset) { (input: Option[Int], f: (Int) => Option[Int], output:Option[Int]) =>
          input flatMap f shouldEqual output
        }
      }
    }

    describe("The orElse function") {

      val dataset =
        Table(
          ("input", "else", "output"),
          (None,    Some(3), Some(3)),
          (Some(2), Some(3), Some(2))
        )

      it("should return else for None and input for not None") {
        forAll (dataset) { (input: Option[Int], elseVal: Option[Int], output: Option[Int]) =>
          input orElse  elseVal shouldEqual output
        }
      }
    }


    describe("The filter function") {

      val f = (x: Int) => x > 0

      val dataset =
        Table(
          ("input", "f", "output" ),
          (None,      f, None     ),
          (Some(-2),  f, None     ),
          (Some(2),   f, Some(2)  )
        )

      it("should filter value") {
        forAll (dataset) { (input: Option[Int], f: (Int) => Boolean, output: Option[Int]) =>
          input filter f shouldEqual output
        }
      }
    }


  }
}
