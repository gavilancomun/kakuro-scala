
package com.flowlikeariver.kakuro {

import org.scalatest._
import Kakuro._

class TestKakuro extends UnitSpec {
  
  "An empty cell" should "draw as hyphens" in {
    assert("   -----  " == draw(e))
  }
  
  "An across cell" should "draw" in {
    assert("   --\\ 9  " == draw(Kakuro.a(9)))
  }
  
  "A down cell" should "draw" in {
    assert("    9\\--  " == draw(Kakuro.d(9)))
  }

  "A down across cell" should "draw" in {
    assert("    9\\ 3  " == draw(da(9, 3)))
  }
}

}
