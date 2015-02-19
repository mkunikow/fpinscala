package fpinscala.gettingstarted

import org.scalatest.{Matchers, FeatureSpec}
import org.scalatest.prop.TableDrivenPropertyChecks._


/**
 * Created by michal on 2/15/15.
 */

class GettingStartedTest  extends FeatureSpec with Matchers {
  feature ("The fibonacci sequence"){
    import fpinscala.gettingstarted.MyModule.fib
    scenario ("should return correct fibonacci number"){

      val dataset =
        Table(
          ("in",    "out"),
          (0,       0    ),
          (1,       1    ),
          (2,       1    ),
          (3,       2    ),
          (4,       3    ),
          (5,       5    )
        )

      forAll (dataset) { (in: Int, out: Int) =>
        fib(in) shouldEqual out
      }
    }
  }

  feature("The isSorted"){
    import fpinscala.gettingstarted.PolymorphicFunctions.isSorted

    scenario("should return true for sorted integer array") {
      val gt = (x: Int,y: Int) => x > y
      val dataset =
        Table(
          ("in",            "f",  "out"),
          (Array[Int](),    gt,    true),
          (Array(1),        gt,    true),
          (Array(1,2,3),    gt,    true),
          (Array(3,2,1),    gt,    false)
        )
      forAll (dataset) { (in: Array[Int], f: (Int,Int) => Boolean, out: Boolean) =>
        isSorted(in, f) shouldEqual out
      }
    }
  }
}
