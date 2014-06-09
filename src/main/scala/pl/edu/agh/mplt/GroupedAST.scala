package pl.edu.agh.mplt

import pl.edu.agh.mplt.parser.declaration.Declaration
import pl.edu.agh.mplt.visitors.{NodeAggregator, NodeMapper}


class GroupedAST(val declarations: Map[String, Stream[Declaration]]) {
   implicit private[this] def map2GroupedAst(map: Map[String, Stream[Declaration]]): GroupedAST =
      new GroupedAST(map)

   def filterGroups(p: (String) => Boolean): GroupedAST =
      declarations.filterKeys(p)

   def filterDeclarations(p: Declaration => Boolean): GroupedAST =
      declarations.map { case (str, ds) => (str, ds.filter(p)) }

   def map(f: PartialFunction[String, NodeMapper]): GroupedAST =
      filterGroups(f.isDefinedAt).declarations.map { case (str, ds) => (str, ds.map(f(str)(_))) }

   def aggregate[B](f: PartialFunction[String, NodeAggregator[B]]): Map[String, Stream[B]] =
      filterGroups(f.isDefinedAt).declarations.map { case (str, ds) => (str, ds.map(f(str)(_))) }
}
