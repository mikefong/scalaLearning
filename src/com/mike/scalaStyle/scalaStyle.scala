package com.mike.scalaStyle

/**
 * Created by chufang on 3/7/15.
 * http://twitter.github.io/effectivescala/index-cn.html
 */
object scalaStyle {
  val xs = List(1,2,3,4)
  xs map {
    case x if x > 5 => "Good"
    case _ => "Bad"
  }

}
