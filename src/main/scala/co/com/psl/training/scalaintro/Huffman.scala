package co.com.psl.training.scalaintro

object Huffman {
  /** Represents a Huffman tree. */
  sealed trait CodeTree extends Product with Serializable
  final case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  final case class Leaf(char: Char, weight: Int) extends CodeTree

  /** For simplicity, we will use Boolean for representing Bits. true = 1   &   false = 0 */
  type Bit = Boolean

  // ------------------------------------------------------------------------------------------- //
  // Part 1: Basics. --------------------------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** Returns the weight of Huffman tree. */
  def weight(tree: CodeTree): Int = ???

  /** Returns the list of characters defined in a given Huffman tree. */
  def chars(tree: CodeTree): List[Char] = ???

  /** Returns the union of two Huffman trees. */
  def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree = ???

  // ------------------------------------------------------------------------------------------- //
  // Part 2: Generating Huffman trees. --------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** Returns the optimal Huffman tree from a String (List of characters). */
  def createCodeTree(chars: List[Char]): CodeTree = ???
  // For implementing this function, first implement the following helpers:

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   */
  def times(chars: List[Char]): List[(Char, Int)] = ???

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights
   * (i.e. the head of the list should have the smallest weight),
   * where the weight of a leaf is the frequency of the character.
   */
  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = ???

  /** Checks whether the list `trees` contains only one single code tree. */
  def singleton(trees: List[CodeTree]): Boolean = ???

  /** Inserts an element in the right place of a sorted ascending list. */
  def insert(elem: CodeTree, list: List[CodeTree]): List[CodeTree] = ???

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = ???

  /** Continuously applies a transformation to a List, until some condition is met. */
  def until(xxx: Any, yyy: Any, zzz: Any): Any = ???
  // Do yourself a favor, and first fix the type signature!

  // ------------------------------------------------------------------------------------------- //
  // Part 3: Encoding. ------------------------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //
  //   Part A: Using a Huffman Tree. ----------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** This function encodes text, using the code tree, into a sequence of bits. */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = ???

  // ------------------------------------------------------------------------------------------- //
  //   Part B: Using a Coding Table (Optional). ------------------------------------------------ //
  // ------------------------------------------------------------------------------------------- //

  /**
   * The previous function is simple, but very inefficient.
   * You goal is now to define quickEncode which encodes an equivalent representation,
   * but more efficiently. By building a CodeTable once, and accessing it latter.
   */
  type CodeTable = List[(Char, List[Bit])] // Is this the best way to encode a lookup table?

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   */
  def convert(tree: CodeTree): CodeTable = ???

  /** This function encodes text, using a code tree, into a sequence of bits. */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = ???

  // ------------------------------------------------------------------------------------------- //
  // Part 4: Decoding. ------------------------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** This function decodes the bit sequence, using a code tree, into a text. */
  def decode(tree: CodeTree)(bits: List[Bit]): List[Char] = ???
}
