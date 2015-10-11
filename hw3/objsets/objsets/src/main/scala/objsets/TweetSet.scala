package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}

abstract class TweetSet {

    def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, this)

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

    def union(that: TweetSet): TweetSet

    def mostRetweeted: Tweet

    def descendingByRetweet: TweetList

    def incl(tweet: Tweet): TweetSet

    def remove(tweet: Tweet): TweetSet

    def contains(tweet: Tweet): Boolean

    def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {
    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = new Empty

    def contains(tweet: Tweet): Boolean = false

    def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

    def remove(tweet: Tweet): TweetSet = this

    def foreach(f: Tweet => Unit): Unit = ()

    def union(that: TweetSet): TweetSet = that

    def mostRetweeted: Tweet = throw new java.util.NoSuchElementException("so sorry, no such elements found :(")

    def descendingByRetweet: TweetList = Nil
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

    def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
        if (p(elem))
            new NonEmpty(elem, left.filter(p), right.filter(p))
        else
            left.filter(p).union(right.filter(p))
    }

    def contains(x: Tweet): Boolean =
        if (x.text < elem.text) left.contains(x)
        else if (elem.text < x.text) right.contains(x)
        else true

    def incl(x: Tweet): TweetSet = {
        if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
        else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
        else this
    }

    def remove(tw: Tweet): TweetSet =
        if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
        else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
        else left.union(right)

    def foreach(f: Tweet => Unit): Unit = {
        f(elem)
        left.foreach(f)
        right.foreach(f)
    }
/*    def union(that: TweetSet): TweetSet = {
        if (that.head())
    }*/
    def union(that: TweetSet): TweetSet = that match {
        case _: Empty => this
        case _ => new NonEmpty(elem, left.union(that.filter(tweet => tweet.text < elem.text)), right.union(that.filter(tweet => tweet.text > elem.text)))
    }

    def mostRetweeted: Tweet = (left, right) match {
        case (_: Empty, _: Empty) =>
            elem
        case (_, _: Empty) =>
            val leftM = left.mostRetweeted
            if (elem.retweets < leftM.retweets)
                leftM
            else
                elem
        case (_: Empty, _) =>
            val rightM = right.mostRetweeted
            if (elem.retweets < rightM.retweets)
                rightM
            else
                elem
        case (_, _) =>
            val leftM = left.mostRetweeted
            val rightM = right.mostRetweeted
            if (elem.retweets > leftM.retweets && elem.retweets >= rightM.retweets)
                elem
            else if (leftM.retweets >= rightM.retweets)
                leftM
            else
                rightM
    }

    def descendingByRetweet: TweetList = new Cons(mostRetweeted, this.remove(mostRetweeted).descendingByRetweet)
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}


object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  lazy val googleTweets: TweetSet = TweetReader.allTweets.filter(t => google.exists(s => t.text.contains(s)))
  lazy val appleTweets: TweetSet = TweetReader.allTweets.filter(t => apple.exists(s => t.text.contains(s)))
  lazy val trending: TweetList = googleTweets.union(appleTweets).descendingByRetweet
}

object Main extends App {
  GoogleVsApple.trending foreach println
}
