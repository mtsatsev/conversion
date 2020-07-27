object Huffman {

  sealed trait Tree[+B] {
    def :^:[B](value: Tree[B]): Node[B] = Node(this,value)
  }
  case class Leaf[B](el: B) extends Tree[B]
  case class Node[B](tr1: Tree[B], tr2: Tree[B]) extends Tree[B]

  sealed trait With[A,B] {
    def upper (): A
    def satelite (): B
    def ==(That:With[A,B]): Boolean
    def <=(That:With[Ordered[A],B]): Boolean
  }

  case class WithAB[A,B](a: A, b: B)extends With[A,B]{

    override def upper(): A = this.a
    override def satelite(): B = this.b
    override def ==(That: With[A, B]): Boolean = this.upper() == That.upper()
    override def <=(That: With[Ordered[A], B]): Boolean = That.upper() <= this.upper()
  }

  def collapse [T](Collapsable: Iterable[List[T]]): T
  def twoTails [T](list: List[T]): List[T] = list.tail.tail

  def frequency[char](cArr: List[char]): List[WithAB[Int,char]] = cArr match {
    case Nil => List()
    case () => List()
    case c => val s = c.sorted.groupBy(x => x).values; List(WithAB(s.map(_.length).head, collapse(s))) ++ frequency(s.tail.toList.flatten)
  }


  def huffman [char](withAB: List[WithAB[Int,char]]): Tree[char] = withAB match{
    case Nil => null
    case x :: () => Leaf(x.satelite())
    case x :: y :: () => Leaf(x.satelite()) :^: Leaf(y.satelite())
    case c =>  val x = c.sorted.map(x => Leaf(x.satelite())).take(2); (x.head :^: x.last) :^: huffman(twoTails(c))
  }

  trait Bit{
    def value():Int
    def show():Char
  }

  case class I() extends Bit {
    override def value(): Int = 1
    override def show(): Char = 'I'
  }

  case class O() extends Bit {
    override def value(): Int = 0
    override def show(): Char = 'O'
  }

  def decode [char](tree: Tree[char], bits: List[Bit]): List[char] = (tree, bits) match {
    case (t, Nil) => List()
    case (t, b) =>

      def letter(tr: Tree[char],lbit: List[Bit]): (List[Bit], char) = (tr,lbit) match {
        case (Leaf(a),x) => (x,a)
        case ((_ :^: trR),(I::x)) => letter(trR,x)
        case ((trL :^: _),(O::x)) => letter(trL,x)
      }

      val (xs,l) = letter(t,b);

      l:: decode(t,xs)
  }
}

/*
  def codes [char](tree: Tree[char]):List[(char, List[Bit])] = {
    val b = List()
    tree match {
      case Leaf(x) => List((x,List[]))
      case (x :^: y) =>
        val exO = (e:char) => (e,codes(x).flatMap(v => O::v._2))
        val exI = (e:char) => (e,codes(x).flatMap(v => I::v._2))

    }
  }
*/

  /*def encode [char] (tree: Tree[Ordering[char]],cArr: List[char]): List[Bit] = (tree, cArr) match {
    case (_, Nil) => List()
    case (t, (c::cs)) =>
      def checkCode(): Unit ={

      }
  }*/

/*
sealed trait Bit extends Enumeration {
  type I(1)
  type O(0)
}

object Bit extends Bit {
  override type I = "I"
  override type O = "O"
}

def codes [char](tree: Tree[char]):List[(char,List[Bit.type ])] = tree match{
  case Leaf(x) => List((x,List[]))
  case (x :^: y) => codes(x).map((a:char,b:List[Bit]) => (a, O::b)  )
}

*/
