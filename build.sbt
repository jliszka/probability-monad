name := "probability-monad" // insert clever name here

scalaVersion := "2.11.1"

crossScalaVersions := Seq("2.9.2", "2.9.3", "2.10.2", "2.11.1")

initialCommands := """
                |import probability_monad._
                |import probability_monad.Distribution._
                |import probability_monad.Examples._""".stripMargin('|')
