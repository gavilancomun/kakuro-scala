
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
  
  "A grid" should "draw" in {
    val g = List(List(e, dd(4), dd(22), e, dd(16), dd(3)), List(e, dd(4), dd(22), e, dd(16), dd(3)))
    assert("\n   -----      4\\--     22\\--     -----     16\\--      3\\--  \n   -----      4\\--     22\\--     -----     16\\--      3\\--  \n" == drawGrid(g))
  }
  
  "a full grid" should "draw" in {
    println(drawGrid(grid1))
  }
  
   "all different" should "result" in {
     assert(true == allDifferent(List(1, 2, 3)))
     assert(false == allDifferent(List(1, 1, 2, 3)))
   }
   
    "permute" should "permute" in {
      assert(List(List(1, 5), List(5, 1), List(3, 3), List(2, 4), List(4, 2)).toSet == permuteAll(List(v(), v()), 6).toSet)
    }
    
    "matrix" should "transpose" in {
      assert(List(List(e, e), List(aa(1), aa(2))) == transpose(List(List(e, aa(1)), List(e, aa(2)))))
    }
    
    "partBy" should "partBy" in {
      assert(List(List(1, 2), List(3, 4, 5)) == partitionBy[Int](x => x < 3, List(1,2,3,4,5)))
    }
    
    "partN" should "part" in {
      assert(List(List(1, 2), List(3)) == partitionN(2, List(1, 2, 3)))
    }
    
    "solver" should "solve" in {
      val expected = "     3         9    \n";
      val solved = solver(grid1)
      val drawSolved = drawGrid(solved)
      assert(expected == drawSolved.substring(drawSolved.length - expected.length))
    }
}

}
