package fpinscala.datastructures

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FeatureSpec, Matchers}


/**
 * Created by michal on 2/15/15.
 */

class ListTest  extends FeatureSpec with Matchers {
  feature ("The tail"){
    import fpinscala.datastructures.List._
    scenario ("should return tail of the list"){

      val dataset =
        Table(
          ("in",    "out"     ),
          (Nil,      Nil      ),
          (List(1),  Nil      ),
          (List(1,2),List(2)  )
        )

      forAll (dataset) { (in: List[Int], out: List[Int]) =>
        tail(in) shouldEqual out
      }
    }
  }
}
