import scala.slick.driver.PostgresDriver.simple._

object TestApp extends App {
	val query = Repository.getBB {
		b => a =>
			b.b2 === 5 && a.a3 === 4
	}
	Console.println(query.selectStatement)
}

object A extends Table[(Int,Int,Int)]("a") {
  def a1 = column[Int]("a1", O.PrimaryKey)
  def a2 = column[Int]("a2")
  def a3 = column[Int]("a3")
  def * = a1 ~ a2 ~ a3
}

object B extends Table[(Int,Int,Int)]("b") {
  def a1 = column[Int]("a1")
  def b1 = column[Int]("b1")
  def b2 = column[Int]("b2")
  def b3 = column[Int]("b3")
  def * = b1 ~ b2 ~ b3
}

object C extends Table[(Int,Int,Int)]("с") {
  def b1 = column[Int]("b1")
  def c1 = column[Int]("c1")
  def c2 = column[Int]("c2")
  def c3 = column[Int]("c3")
  def * = c1 ~ c2 ~ c3
}
object D extends Table[(Int,Int,Int)]("d") {
  def c1 = column[Int]("c1")
  def d1 = column[Int]("d1")
  def d2 = column[Int]("d2")
  def d3 = column[Int]("d3")
  def * = d1 ~ d2 ~ d3
}

object Repository {
	def getA(filter: A.type => Column[Boolean]) = { 
		for {
			a <- A
			if(filter(a))
		} yield a
		
	}
	def getB(filter: B.type => A.type => Column[Boolean]) = {
		for {
			(a, b) <- A.innerJoin(B).on { case (a,b)=>a.a1 === b.a1 }
			if filter(b)(a)
		} yield (b,a)
	}
	def getBB(filter: B.type => A.type => Column[Boolean]) = {
			(for {
				a <- A
			} yield a)
			.innerJoin(B).on { case (a,b) => a.a1 === b.a1 }
			.innerJoin(C).on { case ((a,b),c) => c.b1 === b.b1 }
			.innerJoin(D).on { case (((a,b),c),d) => c.c1 === d.c1 }.map { case (((a,b),c),d) => (a,b,c,d) }
	}
	def getB1(filter: B.type => A.type => Column[Boolean]) = {
		for {
			bref <- B
			(a, b) <- getA(filter(bref)).innerJoin(B).on { case (a,b)=>a.a1 === b.a1 }
		} yield (b,a)
	}
	def getB2(filter: B.type => A.type => Column[Boolean]) = {
		for {
			bref <- B
			(a, b) <- getA(filter(bref)).innerJoin(bref).on { case (a,b)=>a.a1 === b.a1 }
		} yield (b,a)
	}
	def getB3(filter: B.type => A.type => Column[Boolean]) = {
		for {
			b <- B
			a <- getA(filter(b)) if(b.a1 === a.a1)
		} yield (b,a)
	}
}
