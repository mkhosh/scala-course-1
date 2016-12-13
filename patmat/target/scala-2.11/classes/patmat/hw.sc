import patmat.Huffman._

val l1 = List(1,2,4)
val l2 = List(3,5)
l1:::l2

val l = List(('c',6),('a',3),('d',5))
l.sortBy(_._1)

val cs = string2Chars("abccdedasddesa")

cs.contains('z')

times(cs)

decodedSecret

val j0 = List()

val j1 = j0 ::: List(1)
val j2 = j1 ::: List(2)


val encoded = quickEncode(frenchCode)("ag".toList)
decode(frenchCode,encoded)


val t1 = Fork(Leaf('a',2), Leaf('b',3), List('a','b'), 5)
convert(t1)
