package co.com.psl.training.scalaintro

import Anagrams._

class AnagramsSpec extends BaseSpec {
  behavior of "Anagrams"

  it should "compute the occurrences of a word" in {
    wordOccurrences("Hello") shouldBe List(
      'e' -> 1,
      'h' -> 1,
      'l' -> 2,
      'o' -> 1
    )
  }

  it should "compute the occurrences of a sentence" in {
    val sentence = List(
      "What",
      "is",
      "a",
      "man", //?
      "A",
      "miserable",
      "little",
      "pile",
      "of",
      "secrets"
    )
    sentenceOccurrences(sentence) shouldBe List(
      'a' -> 5,
      'b' -> 1,
      'c' -> 1,
      'e' -> 6,
      'f' -> 1,
      'h' -> 1,
      'i' -> 4,
      'l' -> 4,
      'm' -> 2,
      'n' -> 1,
      'o' -> 1,
      'p' -> 1,
      'r' -> 2,
      's' -> 4,
      't' -> 4,
      'w' -> 1
    )
  }

  it should "compute all anagrams of a word" in {
    wordAnagrams("married") should contain theSameElementsAs Set("married", "admirer")
    wordAnagrams("player") should contain theSameElementsAs Set("parley", "pearly", "player", "replay")
  }

  it should "compute only one subset of an empty list, and that is the empty list itself" in {
    combinations(List.empty) shouldBe List(Nil)
  }

  it should "compute the combinations of an occurrence list" in {
    combinations(List(('a', 2), ('b', 2))) should contain theSameElementsAs List(
      List(),
      List(('a', 1)),
      List(('a', 2)),
      List(('b', 1)),
      List(('a', 1), ('b', 1)),
      List(('a', 2), ('b', 1)),
      List(('b', 2)),
      List(('a', 1), ('b', 2)),
      List(('a', 2), ('b', 2))
    )
  }

  it should "substract one occurence list from another" in {
    val adlr = List(('a', 1), ('d', 1), ('l', 1), ('r', 1))
    val r = List(('r', 1))
    subtract(adlr, r) shouldBe List(('a', 1), ('d', 1), ('l', 1))
  }

  it should "compute the anagrams of an empty sentence" in {
    sentenceAnagrams(List.empty) shouldBe List(Nil)
  }

  it should "compute the anagrams of a non-empty sentence" in {
    val sentence = List("Linux", "rulez")
    sentenceAnagrams(sentence) should contain theSameElementsAs List(
      List("Rex", "Lin", "Zulu"),
      List("nil", "Zulu", "Rex"),
      List("Rex", "nil", "Zulu"),
      List("Zulu", "Rex", "Lin"),
      List("null", "Uzi", "Rex"),
      List("Rex", "Zulu", "Lin"),
      List("Uzi", "null", "Rex"),
      List("Rex", "null", "Uzi"),
      List("null", "Rex", "Uzi"),
      List("Lin", "Rex", "Zulu"),
      List("nil", "Rex", "Zulu"),
      List("Rex", "Uzi", "null"),
      List("Rex", "Zulu", "nil"),
      List("Zulu", "Rex", "nil"),
      List("Zulu", "Lin", "Rex"),
      List("Lin", "Zulu", "Rex"),
      List("Uzi", "Rex", "null"),
      List("Zulu", "nil", "Rex"),
      List("rulez", "Linux"),
      List("Linux", "rulez")
    )
  }
}
