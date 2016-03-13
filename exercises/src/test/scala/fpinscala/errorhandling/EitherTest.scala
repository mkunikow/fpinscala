package fpinscala.errorhandling

import org.scalatest.prop.PropertyChecks
import org.scalatest.{FunSpec, Matchers}

import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter


/**
 * Created by michal on 2/15/15.
 */

class EitherTest extends FunSpec with Matchers with PropertyChecks {

  describe("The Either Trait") {

    describe("The map function") {

      val f = (x: Int) => 2 * x
      val dataset =
        Table(
          ("input", "f", "output"),
          (Left("a"), f, Left("a")),
          (Right(2), f, Right(4))
        )

      it("should map option") {
        forAll (dataset) { (input: Either[String, Int], f:(Int)=>Int, output:Either[String, Int]) =>
          input map f shouldEqual output
        }
      }
    }


    describe("The map2 function") {

      val f = (x: Int, y: Int) => x + y
      val dataset =
        Table(
          ("input1", "input2",  "f", "output"),
          (Left("error1"), Left("error2"), f, Left("error1")),
          (Left("error1"), Right(2), f,  Left("error1")),
          (Right(2), Left("error2"), f,  Left("error2")),
          (Right(2),  Right(2),  f, Right(4))
        )

      it("should map option") {
        forAll (dataset) { (input1: Either[String, Int], input2: Either[String, Int], f:(Int, Int)=>Int, output:Either[String, Int]) =>
          input1.map2(input2)(f) shouldEqual output
        }
      }
    }



    describe("The getOrElse function") {

      val dataset =
        Table(
          ("input",         "default",     "output"),
          (Left("error"),    Right(2),      Right(2)),
          (Right(3),         Right(2),      Right(3))
        )

      it("should return Some value for not None and default value for None") {
        forAll (dataset) { (input: Either[String, Int], default: Either[String, Int], output: Either[String, Int]) =>
          input orElse   default shouldEqual output
        }
      }
    }


    describe("The flatMap function") {

      val f = (x: Int) => Right(2 * x)
      val dataset =
        Table(
          ("input",       "f", "output" ),
          (Left("error"),  f,  Left("error")),
          (Right(2),       f,  Right(4) )
        )

      it("should map option") {
        forAll (dataset) { (input: Either[String, Int], f: (Int) => Either[String, Int], output:Either[String, Int]) =>
          input flatMap f shouldEqual output
        }
      }
    }


    describe("The sequence function") {

      import Either.{sequence, sequence_1}
      val dataset =
        Table(
          ("input",                                           "output" ),
          (List[Either[String, Int]](),                       Right(List[Int]()) ),
          (List(Right(2), Left("error1"), Left("error2")),    Left("error1") ),
          (List(Right(2), Right(4)),                          Right(List(2,4)) )
        )

      it("should sequence list") {
        forAll (dataset) { (input: List[Either[String, Int]], output:Either[String, List[Int]]) =>
          sequence(input)  shouldEqual output
        }
      }

      it("sequence_1 should sequence list") {
        forAll (dataset) { (input: List[Either[String, Int]], output:Either[String, List[Int]]) =>
          sequence_1(input)  shouldEqual output
        }
      }
    }


    describe("The traverse function") {

      val f = (x: Int) => Right(2 * x)
      val fe = (x: Int) => Left("error1")

      import Either.{traverse, traverse_1}
      val dataset =
        Table(
          ("input",    "f",  "output" ),
          (List[Int](), f,   Right(List[Int]()) ),
          (List(2, 4),  fe,  Left("error1") ),
          (List(2, 4),  f,   Right(List(4,8)) )
        )

      it("should sequence list") {
        forAll (dataset) { (input: List[Int], f: (Int) => Either[String, Int],  output:Either[String, List[Int]]) =>
          traverse(input)(f)  shouldEqual output
        }
      }


      it("traverse_1 should sequence list") {
        forAll (dataset) { (input: List[Int], f: (Int) => Either[String, Int],  output:Either[String, List[Int]]) =>
          traverse_1(input)(f)  shouldEqual output
        }
      }
    }



  }
}
