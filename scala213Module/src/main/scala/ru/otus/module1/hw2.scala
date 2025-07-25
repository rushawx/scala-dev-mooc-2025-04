package ru.otus.module1

import scala.util.Random


object Main extends App {

  case class hw2() {

    private var Data = List[Int]()

    def init(num: Int): Unit = {
      for (_ <- 0 to num) {
        Data = 0 :: Data
        Data = 1 :: Data
      }
    }

    def extract(): Boolean = {
      val idx = Random.nextInt(Data.size)
      val res = Data(idx)
      Data = Data.patch(idx, Nil, 1)
      res == 1
    }

    def extractTwo(): Boolean = {
      var ans = false
      for (_ <- 0 to 1) {
        if (extract()) {
          ans = true
        }
      }
      ans
    }

    def print(): Unit = {
      Data.foreach( i => println(i))
    }
  }

  var q = 0
  var t = 10000

  for (_ <- 0 to t) {
    val arr = hw2()
    arr.init(3)
    if (arr.extractTwo()) {
      q += 1
    }
  }

  println(q.toDouble / t)
}
