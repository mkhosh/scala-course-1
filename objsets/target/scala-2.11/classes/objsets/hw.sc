import objsets._

val set1 = new Empty
val set2 = set1.incl (new Tweet ("a", "a body", 20) )
val set3 = set2.incl (new Tweet ("b", "b body", 20) )
val c = new Tweet ("c", "c body", 7)
val d = new Tweet ("d", "d body", 9)
val set4c = set3.incl (c)
val set4d = set3.incl (d)
val set5 = set4c.incl (d)


set5.mostRetweetedAcc(c)


val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")


val text = "iOS 6 Adoption At Just Over One Week: 60% For iPhone And 41% For iPad http://t.co/Q0HAgCz8 by @drizzled"

apple.exists(x => text.contains(x))
