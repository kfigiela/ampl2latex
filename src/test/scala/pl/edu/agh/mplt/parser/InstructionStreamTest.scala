package pl.edu.agh.mplt.parser

import org.scalatest.{FlatSpec, Matchers}
import pl.edu.agh.mplt.App

class InstructionStreamTest extends FlatSpec with Matchers {

  "Instruction Stream" should "not run forever" in {
    App.main(Array( """D:\Dropbox\Workspace\Git\MPLT\out\artifacts\MPLT_jar\trainh.mod""", "test_out"))
}
}
