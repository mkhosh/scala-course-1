import funsets.FunSets._

val s1 = singletonSet(1)

val s2 = singletonSet(2)

val s3 = singletonSet(3)


val f = map(union(s3, union(s1, s2)), x => x * x)
printSet(f)
printSet(union(s3, union(s1, s2)))
