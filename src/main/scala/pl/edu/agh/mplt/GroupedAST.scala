package pl.edu.agh.mplt

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.visitors.{NodeAggregator, NodeMapper}
import scala.collection.mutable

class GroupedAST(val declarations: mutable.LinkedHashMap[String, Stream[Declaration]]) {
   implicit private[this] def map2GroupedAst(map: mutable.LinkedHashMap[String, Stream[Declaration]]): GroupedAST =
      new GroupedAST(map)

   def filterGroups(p: (String) => Boolean): GroupedAST ={
     val map = mutable.LinkedHashMap[String, Stream[Declaration]]()
     declarations.foreach{case (key, stream) => if(p(key)) map.put(key, stream)}

     new GroupedAST(map)
   }

   def filterDeclarations(p: Declaration => Boolean): GroupedAST =
      declarations.map { case (str, ds) => (str, ds.filter(p)) }

   def map(f: PartialFunction[String, NodeMapper]): GroupedAST =
      filterGroups(f.isDefinedAt).declarations.map { case (str, ds) => (str, ds.map(f(str)(_))) }

   def aggregate[B](f: PartialFunction[String, NodeAggregator[B]]): mutable.LinkedHashMap[String, Stream[B]] =
      filterGroups(f.isDefinedAt).declarations.map { case (str, ds) => (str, ds.map(f(str)(_))) }
}
