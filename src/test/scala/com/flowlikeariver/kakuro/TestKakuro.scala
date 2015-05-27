
package com.flowlikeariver.kakuro {

import org.scalatest._
import Kakuro._

class TestKakuro extends UnitSpec {
  
  "An empty cell" should "draw as hyphens" in {
    assert("   -----  " == Kakuro.draw(e))
  }
}

}
