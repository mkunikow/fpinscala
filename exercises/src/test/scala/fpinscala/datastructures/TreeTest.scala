package fpinscala.datastructures

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FeatureSpec, Matchers}


/**
 * Created by michal on 2/15/15.
 */

class TreeTest extends FeatureSpec with Matchers {
  feature ("The size function"){
    import fpinscala.datastructures.Tree._
    scenario ("should return the size of the tree"){

      val dataset =
        Table(
          ("tree"                                    ,"size"  ),
          (Leaf(1)                                   , 1      ),
          (Branch(Leaf(1), Leaf(2))                  , 3      ),
          (Branch(Leaf(1), Branch(Leaf(1), Leaf(2))) , 5      )
        )

      forAll (dataset) { (tree: Tree[Int], size: Int) =>
        Tree.size(tree) shouldEqual size
      }
    }
  }


  feature ("The maximum function"){
    import fpinscala.datastructures.Tree._
    scenario ("should return the max value of tree"){

      val dataset =
        Table(
          ("tree"                                    ,"max"   ),
          (Leaf(1)                                   , 1      ),
          (Branch(Leaf(1), Leaf(2))                  , 2      ),
          (Branch(Leaf(1), Branch(Leaf(1), Leaf(4))) , 4      )
        )

      forAll (dataset) { (tree: Tree[Int], max: Int) =>
        Tree.maximum(tree) shouldEqual max
      }
    }
  }


}
