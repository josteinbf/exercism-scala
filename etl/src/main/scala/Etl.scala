object Etl {
    def transform(extracted: Map[Int, Seq[String]]): Map[String, Int] =
        for (
            (value, letters) <- extracted;
            letter <- letters)
        yield (letter.toLowerCase(), value)
}