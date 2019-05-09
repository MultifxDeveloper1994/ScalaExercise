package co.com.psl.training.scalaintro

import scala.io.Source

object Anagrams {
  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically with respect to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /** English dictionary. */
  val dictionary: List[Word] =
    Source
      .fromResource(resource = "dictionary.txt")
      .getLines
      .toList
      .map(line => line.toLowerCase)

  // ------------------------------------------------------------------------------------------- //
  // Part 1: Computing Occurrence Lists. ------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** Given a word, produces its occurrence list. */
  def wordOccurrences(w: Word): Occurrences = {
    w.groupBy(x => x).mapValues(x => x.size).toList
  }

  /** Given a sentence, produces its occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    wordOccurrences(s.mkString(""))
  }

  // ------------------------------------------------------------------------------------------- //
  // Part 2: Computing Anagrams of a Word. ----------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  val dictionaryOcurrencesList = {

    dictionary.map( x =>  ( wordOccurrences(x)  , x) ).groupBy(x => x._1).mapValues(x => x.map(y => y._2))

  }


  /** Returns all anagrams of a word, two words are anagrams if they had the same occurrence list. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryOcurrencesList.getOrElse(wordOccurrences(word)  ,List[Word]())

  //dictionary.filter(w => wordOccurrences(w) == wordOccurrences(word))
  // I am pretty sure there are better ways of doing this, don't you?

  // ------------------------------------------------------------------------------------------- //
  // Part 3: Computing Anagrams of a Sentence. ------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /**
   *  To compute all the anagrams of a sentence, we will need a helper method which:
   *
   *  Given an occurrence list, produces all the subsets of that occurrence list.
   *
   *  Note: There is only one subset of an empty list, and that is the empty list itself.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match{
    case Nil => List(Nil)
    case _ => {
    val newOcurrences = occurrences.flatMap(x => (1 to x._2).map(y => (x._1 , y)) )
       (1 to newOcurrences.size).flatMap(x => newOcurrences.combinations(x)).toList
    }
  }

  /**
   * We now implement another helper method which:
   *
   * Given two occurrence lists `x` and `y`, subtracts the frequencies of the occurrence list `y`
   * from the frequencies of the occurrence list `x`.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = {
        x.toSet.--(y.toSet).toList
  }

  /** Returns all anagrams of a sentence. */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    val occurencesSentence = sentenceOccurrences(sentence)
    val sentenciasCombinadas = combinations(occurencesSentence)

    List[Sentence]()

  }

  // ------------------------------------------------------------------------------------------- //
  // Part 4: Computing Anagrams of a Sentence Optimized (Optional). ---------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** Returns all anagrams of a sentence - (Avoids to recompute values). */
  def sentenceAnagramsMemo(sentence: Sentence): List[Sentence] = ???
}
