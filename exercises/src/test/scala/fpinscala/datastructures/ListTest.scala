package fpinscala.datastructures

import org.scalatest.prop.TableDrivenPropertyChecks._
import org.scalatest.{FeatureSpec, Matchers}


/**
 * Created by michal on 2/15/15.
 */

class ListTest  extends FeatureSpec with Matchers {
  feature ("The tail function"){
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


  feature ("The head function"){
    import fpinscala.datastructures.List._
    scenario ("should return list with new head"){

      val dataset =
        Table(
          ("lst",    "head" , "out"       ),
          (List(1,2), 3,      List(3, 2)  ),
          (List(2),   1,      List(1)     )
        )

      forAll (dataset) { (lst: List[Any], head: Any, out: List[Any]) =>
        setHead(lst, head) shouldEqual out
      }
    }
  }

  feature ("The drop function"){
    import fpinscala.datastructures.List._
    scenario ("should return list with new head"){

      val dataset =
        Table(
          ("lst",        "n",   "out"      ),
          (List(1,2,3),   1,    List(2,3)  ),
          (List(1),       1,    Nil        ),
          (List(),        1,    Nil        ),
          (List(),       -1,    Nil        )
        )

      forAll (dataset) { (lst: List[Any], n: Int, out: List[Any]) =>
        drop(lst, n) shouldEqual out
      }
    }
  }

  feature ("The dropWhile function"){
    import fpinscala.datastructures.List._
    scenario ("should drop all elements while condition is true"){

      val dataset =
        Table(
          ("lst",           "f",               "out"        ),
          (List(-1,-2,3),   (x: Int) => x < 0,  List(3)     ),
          (List(-1,-2,-3),  (x: Int) => x < 0,  List()      ),
          (List(1,2,3),     (x: Int) => x < 0,  List(1,2,3) )
        )

      forAll (dataset) { (lst: List[Int], f: Int => Boolean, out: List[Any]) =>
        dropWhile(lst, f) shouldEqual out
      }
    }
  }


  feature ("The init function"){
    import fpinscala.datastructures.List._
    scenario ("should return all elements except last one"){

      val dataset =
        Table(
          ("lst",            "out"      ),
          (List(1,2,3),       List(1,2) )
        )

      forAll (dataset) { (lst: List[Any], out: List[Any]) =>
        init(lst) shouldEqual out
      }
    }
  }


  feature ("The length function"){
    scenario ("should return the length of the list"){

      val dataset =
        Table(
          ("lst",       "out" ),
          (List(1,2,3),  3    ),
          (List(),       0    )
        )

      forAll (dataset) { (lst: List[Any], out: Int) =>
        List.length(lst) shouldEqual out
      }
    }
  }


  feature ("The reverse function"){
    scenario ("should reverse the list"){

      val dataset =
        Table(
          ("lst",       "out"       ),
          (List(1,2,3),  List(3,2,1)),
          (List(),       List()     )
        )

      forAll (dataset) { (lst: List[Any], out: List[Any]) =>
        List.reverse(lst) shouldEqual out
      }
    }
  }


  feature ("The appendWithFoldRight"){
    scenario ("should append list"){

      val dataset =
        Table(
          ("a1",        "a2"       , "out"),
          (List(1,2),    List(3,4)  , List(1,2,3,4))
        )

      forAll (dataset) { (a1: List[Any], a2: List[Any], out: List[Any]) =>
        List.appendWithFoldRight(a1, a2) shouldEqual out
      }


//      forAll { (a1: List[Int], a2: List[Int]) =>
//        List.appendWithFoldRight(a1, a2) shouldEqual List.append(a1, a2)
//      }
    }
  }


  feature ("The appendWithFoldLeft"){
    scenario ("should append list"){

      val dataset =
        Table(
          ("a1",        "a2"       , "out"),
          (List(1,2),    List(3,4)  , List(1,2,3,4))
        )

      forAll (dataset) { (a1: List[Any], a2: List[Any], out: List[Any]) =>
        List.appendWithFoldLeft(a1, a2) shouldEqual out
      }
    }
  }




}
