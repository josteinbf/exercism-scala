case class WordCount(val phrase: Seq[Char]) {

    private def words: List[String] = {
        def iter(
                acc: (List[String], List[Char]),
                c: Char,
                ): (List[String], List[Char]) = {
            val (ws, buf) = acc
            c match {
                case c if c.isLetterOrDigit || c == '\'' =>
                    (ws, buf ::: List(c))
                case _ => buf match {
                    case Nil => (ws, buf)
                    case _ => (ws ::: List(buf.mkString), List[Char]())
                }
            }
        }
        val (ws, buf) = (phrase ++ " ")
            .foldLeft((List[String](), List[Char]()))(iter)
        ws
            .map(w => if (w.startsWith("\'")) w.tail else w)
            .map(w => if (w.endsWith("\'")) w.init else w)
    }

    def countWords: Map[String, Int] = {
        words
            .map(w => w.toLowerCase)
            .foldLeft(Map[String, Int]())(
                (m, w) => (m + (w -> ((m getOrElse (w, 0)) + 1)))
            )
    }
}


object Foo {
    val wc = new WordCount("hello")
}