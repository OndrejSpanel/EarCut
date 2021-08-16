package net.opengrabeso.earcut4s

import EarCut._

object Demo {
  def main(args: Array[String]): Unit = {
    val data = Array[Double](
      0, 0,
      0, 1,
      1, 1,
      1, 0
    )

    val cut = earcut(data)
    println(cut)
  }
}
