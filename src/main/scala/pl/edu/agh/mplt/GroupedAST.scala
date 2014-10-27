package pl.edu.agh.mplt

import pl.edu.agh.mplt.parser.declaration.{Declaration, InvalidDeclaration}
import pl.edu.agh.mplt.visitors.{NodeAggregator, NodeMapper}

import scala.collection.mutable
import scala.language.implicitConversions

class GroupedAST(val declarations: mutable.LinkedHashMap[String, Stream[Declaration]]) {
  implicit private[this] def map2GroupedAst(map: mutable.LinkedHashMap[String, Stream[Declaration]]): GroupedAST =
    new GroupedAST(map)

  def filterGroups(p: (String) => Boolean): GroupedAST = {
    val map = mutable.LinkedHashMap[String, Stream[Declaration]]()
    val errors: mutable.Buffer[String] = mutable.Buffer[String]()
    declarations.filterKeys(p).foreach { case (k, s) => map.put(k, s)}

    new GroupedAST(map)
  }

  def filterDeclarations(p: Declaration => Boolean): GroupedAST =
    declarations.map { case (str, ds) => (str, ds.filter(p))}

  def map(f: PartialFunction[String, NodeMapper]): GroupedAST =
    filterGroups(f.isDefinedAt).declarations.map { case (str, ds) => (str, ds.map(f(str)(_)))}

  def aggregate(f: PartialFunction[String, NodeAggregator[String]]): mutable.LinkedHashMap[String, Stream[String]] =
    filterGroups(f.isDefinedAt).declarations.map { case (str, ds) => (str, ds.map(f(str)(_)))}


  def errors =
    declarations
      .filter { case ("errors", _) => true; case _ => false}
      .values.flatten
      .map { case InvalidDeclaration(msg) => msg}

  def printErrors(): Unit = errors.foreach(println)

}
