object FlattenArray {
    def flatten(a: Any): List[Any] = {
        a match {
            case l::ls => flatten(l) ::: flatten(ls)
            case Nil => List()
            case null => List()
            case x => List(x)
        }
    }
}