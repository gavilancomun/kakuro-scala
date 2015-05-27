
package com.flowlikeariver.kakuro {

import org.scalatest._
import Kakuro._

class TestKakuro extends UnitSpec {
  
  "An empty cell" should "draw as hyphens" in {
    assert("   -----  " == draw(e))
  }
  
  "An across cell" should "draw" in {
    assert("   --\\ 9  " == draw(aa(9)))
  }
  
  "A down cell" should "draw" in {
    assert("    9\\--  " == draw(dd(9)))
  }

  "A down across cell" should "draw" in {
    assert("    9\\ 3  " == draw(da(9, 3)))
  }

  "A default value cell" should "draw" in {
    assert(" 123456789" == draw(v()))
  }

  "A 2+ value cell" should "draw" in {
    assert(" 1.3.5...9" == draw(v(Set(1, 3, 5, 9))))
  }

  "A 1 value cell" should "draw" in {
    assert("     1    " == draw(v(Set(1))))
  }
}

}
