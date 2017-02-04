import forcomp.Anagrams._

val s = List("eatyou")

val os = sentenceOccurrences(s)

val sub = subtract(os, wordOccurrences("eat"))

val cs = combinations(os)

cs filter (dictionaryByOccurrences contains _)


(for {
  i <- (1 to 10) filter (_ % 3 == 0)
  j <- i to 10
} yield (i, j)).toList

def occurrencesAnagram(occurrences: Occurrences): List[Sentence] = {
  if (occurrences.isEmpty) List(Nil)
  else for {
    occs <- combinations(occurrences) filter (dictionaryByOccurrences contains _)
    others <- occurrencesAnagram(subtract(occurrences, occs))
    word <- dictionaryByOccurrences(occs)
  } yield word :: others
}

occurrencesAnagram(os)

dictionaryByOccurrences contains wordOccurrences("i")
//def sentenceAnagrams(sentence: Sentence): List[Sentence] = {
//  for {
//    occs <- combinations(sentenceOccurrences(sentence))
//    if dictionaryByOccurrences contains os
//
//  }
