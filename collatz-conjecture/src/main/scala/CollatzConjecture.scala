object CollatzConjecture {
    def steps(n: Int): Option[Int] = {

        def iter(n: Int, acc: Int): Int =
           if (n == 1) acc else iter(
               if ((n & 1) == 1) 3*n + 1 else n / 2,
               acc + 1
            )

        if (n < 1) None else Some(iter(n, 0))
    }
}