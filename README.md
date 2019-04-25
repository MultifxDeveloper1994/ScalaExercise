# Programming in Scala - Assignment.

The final evaluative assignment of the Programming in Scala course.

The assignment consists in solving two problems.  
For each problem there will be:

1. A detailed explanation of the problem _(in this document)_.
2. A code template with the basic structure of the program.
3. An _(incomplete)_ set of unit test.

## Problem 1 - Huffman Coding.

Huffman coding is a compression algorithm that can be used to compress lists of characters.

In a normal, uncompressed text, each character is represented by the same number of bits _(usually eight)_.  
In Huffman coding, each character can have a bit representation of a different length,
depending on how common a character is: the characters that appear often in a text
are represented by a shorter bit sequence than those being used more rarely.  
Every Huffman code defines the specific bit sequences used to represent each character.

A Huffman code can be represented by a binary tree whose leaves represent the characters that should be encoded.  
The leaf nodes have associated with them a weight which denotes the frequency of appearance of that character.  
Every branching node of the code tree can be thought of as a set containing
the characters present in the leaves below it.
The weight of a branching node is the total weight of the leaves below it:
this information is necessary for the construction of the tree.

**Encoding**  
For a given Huffman tree, one can obtain the encoded representation of a character
by traversing from the root of the tree to the leaf containing the character.  
Along the way, when a left branch is chosen, a 0 is added to the representation,
and when a right branch is chosen, 1 is added to the representation.

**Decoding**  
Decoding also starts at the root of the tree.
Given a sequence of bits to decode, we successively read the bits,
and for each 0, we choose the left branch, and for each 1 we choose the right branch.  
When we reach a leaf, we decode the corresponding character and then start again at the root of the tree.

## Problem 2 - Anagrams.

An anagram of a word is a rearrangement of its letters such that a word with a different meaning is formed.  
For example, if we rearrange the letters of the word `Elvis` we can obtain the word `lives`, which is one of its anagrams.

In a similar way, an anagram of a sentence is a rearrangement of all the characters in the sentence
such that a new sentence is formed. The new sentence consists of meaningful words, the number of which
may or may not correspond to the number of words in the original sentence.  
For example, the sentence: `I love you`, is an anagram of the sentence: `You olive`.
In this exercise, we will consider permutations of words anagrams of the sentence, for example: `You I love`.

When producing anagrams, we will ignore character casing and punctuation characters.

Here is the general idea. We will transform the characters of the sentence into a list saying how often each character appears.
We will call this list the occurrence list. To find anagrams of a word we will find all the words from the dictionary
which have the same occurrence list.  
Finding an anagram of a sentence is slightly more difficult. We will transform the sentence into its occurrence list,
then try to extract any subset of characters from it to see if we can form any meaningful words.
From the remaining characters we will solve the problem recursively and then combine all the meaningful words
we have found with the recursive solution.

-------------------------------------------------
-------------------------------------------------

<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>
<br>

-------------------------------------------------

##### Secret Message Puzzle.

<sub><sup><sub><i>
Hey, if you are reading this, I have big news for you!<br>
This is your first step for solving a little puzzle I made for you guys!<br>
The objective is simple, you need to find a secret message I left hidden in this project.<br>
The first one in achieving it, will win a cool reward.<br>
<sub>eulc tsrif eht dnif lliw uoy tnemucod siht fo ecruos eht nI</sub>
</i></sub></sup></sub>

-------------------------------------------------
-------------------------------------------------
































































[//]: # ( The secret message is: "The _ is _ _, it _ _ even _")


[//]: # ( "The _ is" => "State ship" )


[//]: # ( "is _ _" => "aXMgbmV2ZXIgZGVhZA==" )


[//]: # ( "it _ _ even _" => "1011000010010110110110010010110101100000010010110011011001001010010010111111011100110000" )



























































































