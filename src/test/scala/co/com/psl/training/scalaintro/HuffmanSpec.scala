package co.com.psl.training.scalaintro

import Huffman._

class HuffmanSpec extends BaseSpec {
  behavior of "Huffman Coding"

  /**
   * A Huffman coding tree for the English language.
   *
   * Generated from the data given at:
   *   https://en.wikipedia.org/wiki/Letter_frequency#Relative_frequencies_of_letters_in_the_English_language
   */
  val englishCode: CodeTree =
    Fork(Fork(Fork(Leaf('t', 90), Fork(Leaf('l', 50), Leaf('r', 50), List('l', 'r'), 100), List('t', 'l', 'r'), 190),
    Fork(Fork(Fork(Leaf('w', 25), Fork(Fork(Fork(Leaf('x', 2), Fork(Leaf('z', 1), Fork(Leaf('j', 1), Leaf('q', 1),
    List('j', 'q'), 2), List('z', 'j', 'q'), 3), List('x', 'z', 'j', 'q'), 5), Leaf(' ', 10), List('x', 'z', 'j', 'q', ' '), 15),
    Leaf('b', 15), List('x', 'z', 'j', 'q', ' ', 'b'), 30), List('w', 'x', 'z', 'j', 'q', ' ', 'b'), 55),
    Fork(Leaf('u', 30), Leaf('c', 30), List('u', 'c'), 60), List('w', 'x', 'z', 'j', 'q', ' ', 'b', 'u', 'c'), 115),
    Fork(Leaf('s', 60), Leaf('h', 60), List('s', 'h'), 120), List('w', 'x', 'z', 'j', 'q', ' ', 'b', 'u', 'c', 's', 'h'), 235),
    List('t', 'l', 'r', 'w', 'x', 'z', 'j', 'q', ' ', 'b', 'u', 'c', 's', 'h'), 425), Fork(Fork(Leaf('e', 120),
    Fork(Leaf('n', 65), Leaf('i', 70), List('n', 'i'), 135), List('e', 'n', 'i'), 255), Fork(Fork(Leaf('o', 75),
    Fork(Fork(Leaf('f', 20), Leaf('g', 20), List('f', 'g'), 40), Fork(Fork(Leaf('v', 10), Leaf('k', 10),
    List('v', 'k'), 20), Leaf('y', 20), List('v', 'k', 'y'), 40), List('f', 'g', 'v', 'k', 'y'), 80),
    List('o', 'f', 'g', 'v', 'k', 'y'), 155), Fork(Leaf('a', 80), Fork(Leaf('d', 40), Fork(Leaf('p', 20),
    Leaf('m', 25), List('p', 'm'), 45), List('d', 'p', 'm'), 85), List('a', 'd', 'p', 'm'), 165),
    List('o', 'f', 'g', 'v', 'k', 'y', 'a', 'd', 'p', 'm'), 320), List('e', 'n', 'i', 'o', 'f', 'g', 'v', 'k', 'y', 'a', 'd', 'p', 'm'), 575),
    List('t', 'l', 'r', 'w', 'x', 'z', 'j', 'q', ' ', 'b', 'u', 'c', 's', 'h', 'e', 'n', 'i', 'o', 'f', 'g', 'v', 'k', 'y', 'a', 'd', 'p', 'm'), 1000)

  it should "extract the weight of a Tree" in {
    val t1 = Leaf(char   = 'x', weight = 3)
    val t2 = Fork(
      left   = Leaf(char   = 'x', weight = 3),
      right  = Leaf(char   = 'z', weight = 2),
      chars  = List('x', 'z'),
      weight = 5
    )

    weight(t1) shouldBe 3
    weight(t2) shouldBe 5
    weight(englishCode) shouldBe 1000
  }

  it should "extract the chars of a Tree" in {
    val t1 = Leaf(char   = 'x', weight = 3)
    val t2 : CodeTree = Fork(
      left   = Leaf(char   = 'x', weight = 3),
      right  = Leaf(char   = 'z', weight = 2),
      chars  = List('x', 'z'),
      weight = 5
    )

    chars(t1) shouldBe List('x')
    chars(t2) shouldBe List('x', 'z')
    chars(englishCode) should contain theSameElementsAs List(
      'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',
      'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
      's', 't', 'u', 'v', 'w', 'x', 'y', 'z', ' '
    )
  }

  it should "union two code trees into one" in {
    val left = Leaf(char   = 'x', weight = 3)
    val right = Leaf(char   = 'z', weight = 2)

    makeCodeTree(left, right) shouldBe Fork(
      left   = Leaf(char   = 'x', weight = 3),
      right  = Leaf(char   = 'z', weight = 2),
      chars  = List('x', 'z'),
      weight = 5
    )
  }

  it should "encode and decode text" in {
    val message = "Hello, World!".toList
    val tree = createCodeTree(message)
    val encoded = encode(tree)(message)
    val decoded = decode(tree)(encoded)

    decoded shouldBe message
  }

  it should "encode and decode text Quick" in {
    val message = "Hello, World!".toList
    val tree = createCodeTree(message)
    val encoded = quickEncode(tree)(message)
    val decoded = decode(tree)(encoded)

    decoded shouldBe message
  }

}
