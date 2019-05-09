package co.com.psl.training.scalaintro

import scala.annotation.tailrec

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
  def weight(tree: CodeTree): Int = tree match
  {
      case Leaf(_ , x) => x;
      case Fork(l,r, _ ,_) =>  weight(l) + weight(r)
  }

  /** Returns the list of characters defined in a given Huffman tree. */
  def chars(tree: CodeTree): List[Char] =  tree match {
    case Leaf(x , _) => List(x);
    case Fork(left,right, _ ,_) =>  chars(left) ++ chars(right)
  }

  /** Returns the union of two Huffman trees. */
  def makeCodeTree(left: CodeTree, right: CodeTree): CodeTree = {
    Fork(left,right , chars(left) ++ chars(right)  ,weight(left) + weight(right))
  }

  // ------------------------------------------------------------------------------------------- //
  // Part 2: Generating Huffman trees. --------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** Returns the optimal Huffman tree from a String (List of characters). */
  def createCodeTree(chars: List[Char]): CodeTree = {

  combine(makeOrderedLeafList(times(chars)))(0)

  }
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
  def times(chars: List[Char]): List[(Char, Int)] = {
    chars.groupBy(x => x).mapValues(x => x.size).toList
  }

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights
   * (i.e. the head of the list should have the smallest weight),
   * where the weight of a leaf is the frequency of the character.
   */

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    freqs.map(x => Leaf(x._1 , x._2)).sortWith((leaf1 , leaf2)=>  weight(leaf1) < weight(leaf2))
  }

  /** Checks whether the list `trees` contains only one single code tree. */
  def singleton(trees: List[CodeTree]): Boolean = {
    trees.size == 1
  }

  /** Inserts an element in the right place of a sorted ascending list. */
  def insert(elem: CodeTree, list: List[CodeTree]): List[CodeTree] =  {
      @tailrec
      def loop( elem: CodeTree ,  list : List[CodeTree]  , acc : List[CodeTree] = List[CodeTree]()) : List[CodeTree] = list match {
        case Nil =>   acc :+ elem
        case head :: tail => {
          if(weight(elem) <= weight(head)){
           acc ::: elem :: head :: tail
          }
          else{ loop(elem , tail ,  acc :+ head) }
        }
  }

    loop(elem , list)

  }

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
  @tailrec
  def combine(trees: List[CodeTree]): List[CodeTree] = {


    if(singleton(trees)) trees
    else {
      val head = trees.head
      val tail = trees.tail
      combine(insert(makeCodeTree(head , tail.head) , tail.tail))

    }

  }

  /** Continuously applies a transformation to a List, until some condition is met. */

  def until[A](list: List[A], transformation: A => A, condition: A => Boolean): List[A] = {

    @tailrec
    def loop[A]( list: List[A] ,  transformation : A => A , condition: A => Boolean ,  acc :  List[A] = List[A]()) : List[A] = list match {
      case Nil =>    acc
      case head :: tail => {
        if(condition(head)){
          loop(tail , transformation, condition , acc :+ transformation(head))
        }
        else{
          acc ::: head :: tail
        }
      }
      }
    loop(list,transformation,condition)
  }


  // Do yourself a favor, and first fix the type signature!

  // ------------------------------------------------------------------------------------------- //
  // Part 3: Encoding. ------------------------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //
  //   Part A: Using a Huffman Tree. ----------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  // TODO. tailrec
  def encodeChar(tree: CodeTree)(text: Char): List[Bit] = tree match{

    case Leaf(_,_) => List[Bit]();
    case Fork(left,right,_,_) => {
      if(chars(left).contains(text))
      { false :: encodeChar(left)(text) }
      else{
        true :: encodeChar(right)(text)
      }
    }
  }


  /** This function encodes text, using the code tree, into a sequence of bits. */
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    @tailrec
    def loop(tree: CodeTree , text: List[Char] , acc : List[Bit]  = List[Bit] () ) : List[Bit] = text match{
        case Nil => acc
        case head :: tail => loop(tree , tail , acc ::: encodeChar(tree)(head))
    }
    loop(tree,text)
  }

  // ------------------------------------------------------------------------------------------- //
  //   Part B: Using a Coding Table (Optional). ------------------------------------------------ //
  // ------------------------------------------------------------------------------------------- //


  /**
   * The previous function is simple, but very inefficient.
   * You goal is now to define quickEncode which encodes an equivalent representation,
   * but more efficiently. By building a CodeTable once, and accessing it latter.
   */
  type CodeTable = Map[Char, List[Bit]] // Is this the best way to encode a lookup table?
  type CodeTableInverse = Map[ List[Bit] , Char]
  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   */
  def convert(tree: CodeTree): CodeTable = {
    chars(tree).map( x =>  (x , encodeChar(tree)(x))).toMap

  }

  /** This function encodes text, using a code tree, into a sequence of bits. */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val treeConvertedMap = convert(tree)
    @tailrec
    def loop(tree: CodeTree , text: List[Char] , acc : List[Bit]  = List[Bit] () ) : List[Bit] = text match{
      case Nil => acc
      case head :: tail => loop(tree , tail , acc ::: treeConvertedMap(head))
    }
    loop(tree,text)

  }

  def convertInverse(tree: CodeTree): CodeTableInverse = {
    convert(tree).map( x => (x._2 -> x._1))
  }

  // ------------------------------------------------------------------------------------------- //
  // Part 4: Decoding. ------------------------------------------------------------------------- //
  // ------------------------------------------------------------------------------------------- //

  /** This function decodes the bit sequence, using a code tree, into a text. */
  def decode(tree: CodeTree)(bits: List[Bit]): List[Char] = {
     val mapInverse  = convertInverse(tree);
   @tailrec
    def loop(bits: List[Bit] , acc : List[Char] = List[Char]() , acc2 : List[Bit] = List[Bit]()) : List[Char]  =  bits match {
       case Nil => acc
       case head :: tail => {
         if(mapInverse.contains(acc2 :+ head)){
           val value = mapInverse.get(acc2 :+ head).get
           loop(tail , acc :+ value , List[Bit]())
         }
         else{
           loop(tail , acc, acc2 :+ head)
         }
       }
   }
   loop(bits)
  }
}
