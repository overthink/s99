// S-99 problems: http://aperiodic.net/phil/scala/s-99/

// find the last element of a list
// CHEATED (needed to see how answers should be structured)
object P01 {
  // cheap way
  def last_alt1[T](l: List[T]): T = l.last

  // recurse on tails until there isn't one
  def last[T](l: List[T]): T = l match {
    case e :: Nil => e
    case _ :: tail => last(tail)
    case _ => throw new NoSuchElementException
  }
}


// second last item
object P02 {
  // mine
  def penultimate[T](xs: List[T]): T = xs match {
    // p - penultimate, l - last
    case p :: l :: Nil => p
    case p :: l :: tail => penultimate(tail)
    case _ => throw new NoSuchElementException
  }

  // theirs 1
  def penultimate_alt1[T](xs: List[T]): T = xs match {
    case e :: _ :: Nil => e
    case _ :: tail => penultimate_alt1(tail)
    case _ => throw new NoSuchElementException
  }

  // theirs builtin
  def penultimate_alt2[T](xs: List[T]): T =
    if (xs.size < 2)
      throw new NoSuchElementException
    else
      xs.takeRight(2).head
}


// kth element of a list
object P03 {
  // mine (cheap)
  def nth[T](n: Int, xs: List[T]): T = {
    xs(n)
  }

  // theirs (neat)
  def nth_alt1[T](n: Int, xs: List[T]): T = (n, xs) match {
    case (0, e :: _) => e
    case (n, e :: tail) => nth_alt1(n - 1, tail)
    case (_, Nil) => throw new NoSuchElementException
  }
}


// number of elements in a list
object P04 {
  // mine
  def length[T](xs: List[T]): Int = xs match {
    case e :: Nil => 1
    case _ :: rest => 1 + length(rest)
    case _ => 0
  }

  // better simple recursion
  def length_alt1[T](xs: List[T]): Int = xs match {
    case Nil => 0
    case _ :: tail => 1 + length_alt1(tail)
  }

  // with fold
  def length_alt2[T](xs: List[T]): Int = xs.foldLeft(0) { (c, _) => c + 1 }

  // tail recursive
  def length_alt3[T](xs: List[T]): Int = {
    def lengthR(result: Int, curList: List[T]): Int = curList match {
      case Nil => result
      case _ :: tail => lengthR(result + 1, tail)
    }
    lengthR(0, xs)
  }
}


// reverse a list
object P05 {
  // mine
  def reverse[T](xs: List[T]): List[T] = xs match {
    case e :: Nil => List(e)
    case head :: tail => reverse(tail) ::: List(head)
    case _ => Nil
  }

  // better recursive
  def reverse_alt1[T](xs: List[T]): List[T] = xs match {
    case Nil => Nil
    case head :: tail => reverse(tail) ::: List(head)
  }
}


// palindromes
object P06 {
  // mine
  def isPalindrome[T](xs: List[T]): Boolean = xs match {
    case Nil => false
    case List(_) => true
    case List(e1, e2) => e1 == e2
    case _ if xs.first == xs.last => isPalindrome(xs.slice(1, xs.size - 1))
    case _ => false
    //case List(e1, middle @ _*, e2) if e1 == e2 => isPalindrome(List(middle))
    //case _ => false
  }
}


// flatten lists
object P07 {
  // mine
  def flatten(xs: List[Any]) = {
    def flattenR(accum: List[Any], xs: List[Any]): List[Any] = {
      var result = accum
      xs.foreach { x =>
        x match {
          case e: List[_] => result = flattenR(result, e)
          case e => result = result ::: List(e)
        }
      }
      result
    }
    flattenR(List[Any](), xs)
  }

  // theirs (I considerred the use of flatMap to be cheap...)
  def flatten_alt1(xs: List[Any]): List[Any] = xs flatMap {
    case xs: List[_] => flatten(xs)
    case e => List(e)
  }
}


// Eliminate consecutive duplicates of list elements.
object P08 { 
  // mine (same as theirs! I'm learnDing)
  def compress(xs: List[Any]): List[Any] = xs match {
    case head :: tail => head :: compress(tail.dropWhile(_ == head))
    case _ => Nil
  }
}


// Pack consecutive duplicates of list elements into sublists.
object P09 {
  // mine
  def pack[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case e :: tail =>
      val result = xs.span(_ == e)
      result._1 :: pack(result._2)
  }

  // theirs (same as mine, but Nil -> List(List()), which I don't think is required)
  def pack_alt1[T](l: List[T]): List[List[T]] = {
    if (l.isEmpty) List(List())
    else {
      val (packed, next) = l.span(_ == l.head)
      if (next == Nil) List(packed)
      else packed :: pack(next)
    }
  }
}


// Run-length encoding of a list.
object P10 {
  // mine
  def encode[T](xs: List[T]): List[(Int, T)] =
    P09.pack(xs).map(x => (x.size, x.first))

  // theirs is exactly the same, but uses .length and .head instead of .size and .first
}


// Modify the result of problem P10 in such a way that if an element has no
// duplicates it is simply copied into the result list. Only elements with
// duplicates are transferred as (N, E) terms.
object P11 {
  // mine
  def encodeModified[T](xs: List[T]): List[Any] =
    P10.encode(xs).map { x => 
      x match {
        case (1, e) => e
        case _ => x
      }
    }

  // theirs (same, different syntax)
  def encodeModified_alt1[T](l: List[T]): List[Any] =
    P10.encode(l).map(t => if (t._1 == 1) t._2 else t)

}



