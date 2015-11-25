package patmat

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import patmat.Huffman._

@RunWith(classOf[JUnitRunner])
class HuffmanSuite extends FunSuite {
	trait TestTrees {
		val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
		val t2 = Fork(Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5), Leaf('d',4), List('a','b','d'), 9)

    val characters = List('c', 'a', 'b', 'a', 'b', 'a')
    val tree = Fork(
                    Leaf('a', 3),
                    Fork( Leaf('c', 1), 
                          Leaf('b', 2), 
                          List('c', 'b'), 
                          3),  
                    List('a', 'c', 'b'), 
                    6
                    )
    val code: List[Bit] = List(1,0,1,1,0,0,1)
    val text =  List('a', 'c', 'a', 'b', 'a')
    val codeTable: CodeTable = List(('a', List(1)), ('c', List(0,1)), ('b', List(0,0)))
	}


  test("weight of a larger tree") {
    new TestTrees {
      assert(weight(t1) === 5)
    }
  }


  test("chars of a larger tree") {
    new TestTrees {
      assert(chars(t2) === List('a','b','d'))
    }
  }


  test("string2chars(\"hello, world\")") {
    assert(string2Chars("hello, world") === List('h', 'e', 'l', 'l', 'o', ',', ' ', 'w', 'o', 'r', 'l', 'd'))
  }


  test("times works properly") {
    val list = List('c', 'b', 'a', 'c')
    assert(times(list) == List(('a', 1), ('b', 1), ('c', 2)))
  }

  test("makeOrderedLeafList for some frequency table") {
    assert(makeOrderedLeafList(List(('t', 2), ('e', 1), ('x', 3))) === List(Leaf('e',1), Leaf('t',2), Leaf('x',3)))
  }

  test("singleton behaves as expected") {
    new TestTrees {
      val list_t1     = List(t1)
      val list_t1_t2  = List(t1, t2)
      assert(!singleton(List()))
      assert(singleton(list_t1))
      assert(!singleton(list_t1_t2))
    }
  }

  test("combine and until of some leaf list") {
    val leaflist = List(Leaf('e', 2), Leaf('t', 3), Leaf('x', 4), Leaf('z', 6))
    val finalTreeList = List(Fork(
                                      Leaf('z', 6),
                                      Fork(
                                            Leaf('x', 4),
                                            Fork( 
                                                  Leaf('e', 2),
                                                  Leaf('t', 3),
                                                  List('e', 't'),
                                                  5),
                                            List('x', 'e', 't'),
                                            9),
                                      List('z', 'x', 'e', 't'),
                                      15
                                      )
                                  )
    assert(combine(leaflist) === finalTreeList)
    assert(until(singleton, combine)(leaflist) === finalTreeList)
  }

  test("createCodeTree") {
    new TestTrees {
       assert(createCodeTree(characters) ===  tree)
    }
  }

  test("decode and encode work properly") {
    new TestTrees {
      assert(decode(tree, code) === text)
      assert(getCharacterEncoding(tree)('a') === List(1))
      assert(getCharacterEncoding(tree)('b') === List(0, 0))
      assert(getCharacterEncoding(tree)('c') === List(0, 1))
      assert(encode(tree)(text) === code)
    }
  }

  test("decode and encode a very short text should be identity") {
    new TestTrees {
      assert(decode(t1, encode(t1)("ab".toList)) === "ab".toList)
    }
  }

  test("codeBits works properly") {
    new TestTrees {
      assert(codeBits(codeTable)('a') === List(1))
      assert(codeBits(codeTable)('b') === List(0,0))
      assert(codeBits(codeTable)('c') === List(0,1))
    }
  }

  test("convert works properly") {
    new TestTrees {
      assert(convert(tree) === codeTable)
    }
  }

  test("quickEncode works properly") {
    new TestTrees {
      assert(quickEncode(tree)(text) === code)
    }
  }
}


















































