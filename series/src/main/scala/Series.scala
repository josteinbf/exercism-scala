object Series {
    def slices[T](length: Int, seq: Seq[Char]): List[List[Int]] = {
        if (seq.length < length)
            Nil
        else
            seq.take(length).map(c => c.toString.toInt).toList ::
            slices(length, seq.tail)
    }
}