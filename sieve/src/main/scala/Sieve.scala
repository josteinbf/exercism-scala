object Sieve {
    def primes(N: Int): List[Int] = {

        def iter(candidates: List[Int]): List[Int] = {
            candidates match {
                case Nil => Nil
                case c::cs => {
                    def sieve(m_ys: (Int, List[Int]), x: Int): (Int, List[Int]) = {
                        val (m, ys) = m_ys
                        x match {
                            case x if x < m =>
                                (m, ys ::: List(x))   // same multiple, keep x
                            case x if x > m =>
                                (m + c, ys ::: List(x)) // next multiple, keep x
                            case m => (m + c, ys)  // next multiple, throw away x
                        }
                    }

                    val filtered: (Int, List[Int]) =
                        cs.foldLeft((c, List[Int]()))(sieve)
                    c :: iter(filtered._2)
                }
            }
        }
        iter((2 to N).toList)
    }
}