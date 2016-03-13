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


    describe("The sequenceViaFoldRight") {

      import Option.{variance, variance2}

      val dataset =
        Table(
          ("input",                              "output"       ),
          (Seq(),                                 None          ),
          (Seq(600.0,470.0,170.0,430.0,300.0),    Some(21704.0)  )
        )

      it("should filter value for variance") {
        forAll (dataset) { (input: Seq[Double], output: Option[Double]) =>
          variance(input) shouldEqual output
        }
      }


      it("should filter value for variance2") {
        forAll (dataset) { (input: Seq[Double], output: Option[Double]) =>
          variance2(input) shouldEqual output
        }
      }
    }



    describe("The map2") {

      import Option.{map2, map2ViaFor}

      val f = (x: Int, y:Int) => x + y


      val dataset =
        Table(
          ("input1",    "input2",  "f",       "output"      ),
          (None,         None,     f,        None           ),
          (Some(1),      Some(2),  f,        Some(3)        )
        )

      it("should apply f to input arguments") {
        forAll (dataset) { (input1: Option[Int], input2: Option[Int], f: (Int, Int) => Int, output: Option[Int]) =>
          map2(input1, input2)(f) shouldEqual output
        }
      }


      it(" via for should apply f to input arguments") {
        forAll (dataset) { (input1: Option[Int], input2: Option[Int], f: (Int, Int) => Int, output: Option[Int]) =>
          map2ViaFor(input1, input2)(f) shouldEqual output
        }
      }
    }

    describe("The sequence function") {

      import Option.{sequenceViaFoldRight, sequence, sequence_1}

      val dataset =
        Table(
          ("input",                              "output"         ),
          (List[Option[Int]](),                   Some(List())    ),
          (List(Some(1), Some(2), None, Some(3)), None            ),
          (List(Some(1), Some(2)),                Some(List(1,2)) )
        )

      it("sequenceViaFoldRight should convert List[Option[A]] to Option[List[A]]") {
        forAll (dataset) { (input: List[Option[Int]], output: Option[List[Int]]) =>
          sequenceViaFoldRight(input) shouldEqual output
        }
      }


      it("sequence should convert List[Option[A]] to Option[List[A]]") {
        forAll (dataset) { (input: List[Option[Int]], output: Option[List[Int]]) =>
          sequence(input) shouldEqual output
        }
      }


      it("sequence_1 should convert List[Option[A]] to Option[List[A]]") {
        forAll (dataset) { (input: List[Option[Int]], output: Option[List[Int]]) =>
          sequence_1(input) shouldEqual output
        }
      }
    }


    describe("The traverse function") {

      import Option.traverseViaFoldRight

      val f = (x: String) => {
        try Some(x.toInt)
        catch {case e: Exception => None}
      }


      val dataset =
        Table(
          ("input",                  "f",       "output"        ),
          (List[String](),            f,        None            ),
          (List("1", "2", "a"),       f,        None            ),
          (List("1", "2"),            f,        Some(List(1,2)) )
        )

      it("should travers list applying function f") {
        forAll (dataset) { (input: List[String], f: (String) => Option[Int], output: Option[List[Int]]) =>
          traverseViaFoldRight(input)(f) shouldEqual output
        }
      }
    }


  }
}
