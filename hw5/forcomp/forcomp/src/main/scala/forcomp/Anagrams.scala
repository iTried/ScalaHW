package forcomp

import common._

object Anagrams {

  type Word = String

  type Sentence = List[Word]

  type Occurrences = List[(Char, Int)]

  val dictionary: List[Word] = loadDictionary

  def wordOccurrences(w: Word): Occurrences = w.toLowerCase().toList.groupBy(x => x).mapValues(_.length).toList.sorted

  def sentenceOccurrences(s: Sentence): Occurrences = wordOccurrences((s foldLeft "") (_ + _))

  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] = dictionary.groupBy( w => wordOccurrences(w))

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] = dictionaryByOccurrences(wordOccurrences(word))

  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case Nil => List(Nil)
    case x::xs => (for{
      i <- 0 to x._2
      a <- combinations(xs)
    } yield if(i==0) a else (x._1,i) :: a).toList
  }

  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
    val minusy = y map (y => (y._1, -y._2))
    (x ::: minusy).groupBy(_._1).mapValues(_.map(_._2).sum).toList.filter(_._2 != 0).sorted
  }

  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
    def sentenceAnagrams(sentence: Occurrences):List[Sentence] = {
      if(sentence.isEmpty)
        List(List())
      else
        for(
          s <- combinations(sentence).filter(dictionaryByOccurrences.contains);
          t <- dictionaryByOccurrences(s);
          p <-  sentenceAnagrams(subtract(sentence,s))
        )yield t :: p
    }
    sentenceAnagrams(sentenceOccurrences(sentence))
  }
}
