/*
 * Copyright (c) 2002-2019 "Neo4j,"
 * Neo4j Sweden AB [http://neo4j.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.cypher.docgen

import java.io.File

import org.neo4j.cypher.docgen.tooling.{DocBuilder, DocumentingTest, ResultAssertions}

import scala.collection.mutable

class LoadCSVFunctionsTest extends DocumentingTest {
  override def outputPath = "target/docs/dev/ql/functions"
  val path: File = new File(outputPath)

  val dir: File = createDir("load-csv")

  implicit var csvFilesDir: File = createDir(dir, "csv-files")

  def createDir(folder: String): File = createDir(path, folder)

  private def createDir(where: File, folder: String): File = {
    val dir = new File(where, folder)
    if (!dir.exists()) {
      dir.mkdirs()
    }
    dir
  }

  override def doc = new DocBuilder {
    private val csvA = new CsvFile("a.csv").withContentsF(
      Seq("1", "ABBA", "1992"),
      Seq("2", "Roxette", "1986"),
      Seq("3", "Europe", "1979"),
      Seq("4", "The Cardigans", "1992")
    )

    private val csvB = new CsvFile("b.csv").withContentsF(
      Seq("1", "ABBA", "1992"),
      Seq("2", "Roxette", "1986"),
      Seq("3", "Europe", "1979"),
      Seq("4", "The Cardigans", "1992")
    )

    private val csvC = new CsvFile("c.csv").withContentsF(
      Seq("1", "ABBA", "1992"),
      Seq("2", "Roxette", "1986"),
      Seq("3", "Europe", "1979"),
      Seq("4", "The Cardigans", "1992")
    )
    val fileUrls: mutable.Map[String,String] = mutable.Map()

    private def mapFilePath(message: String): String =
      fileUrls.foldLeft(message)((acc, entry) => acc.replace(entry._1, entry._2))

    Seq(csvA, csvB, csvC).foreach { c =>
      val path = CsvFile.urify(c)
      val url = "{csv-dir}/" + c.getName
      val short = path.replace("file:", "")
      fileUrls(path) = url
      fileUrls(short) = url
    }

    doc("Load CSV functions", "query-functions-load-csv")
    synopsis("These functions are used to provide the current file name and line number within a `LOAD CSV` command.")
    p(
      """Functions:
        |
        |* <<functions-filename,filename()>>
        |* <<functions-linenumber,linenumber()>>
      """.stripMargin)
    p("The `LOAD CSV` command is described in more detail in the section <<query-load-csv>>. ")
    section("filename()", "functions-filename") {
      p(
        """`filename()` returns a string representing the file name of the currently importing CSV file.
           If no file is being imported, `null` will be returned.
        """.stripMargin)
      function("filename()", "A String.")
      considerations("If this function is called outside the context of `LOAD CSV`, then the return value will be `null`.")
      preformattedQueryX(
        s"""UNWIND [
           |  '${CsvFile.urify(csvA)}',
           |  '${CsvFile.urify(csvB)}',
           |  '${CsvFile.urify(csvC)}'] as csv
           |LOAD CSV FROM csv AS line
           |CREATE (n:Artist {name: line[1], year: toInteger(line[2])})
           |RETURN filename() AS file, n.name AS artist, n.year AS born""".stripMargin, mapFilePath, ResultAssertions(r => {
          r.toList.head("file").toString should include("a.csv")
        })) {
        p("The filename used for each node created is shown.")
        resultTable()
      }
    }
    section("linenumber()", "functions-linenumber") {
      p(
        """`linenumber()` returns an integer representing the line number within the currently importing CSV file.
           If no file is being imported, `null` will be returned.
        """.stripMargin)
      function("linenumber()", "An Integer.")
      considerations("If this function is called outside the context of `LOAD CSV`, then the return value will be `null`.")
      preformattedQueryX(
        s"""UNWIND [
           |  '${CsvFile.urify(csvA)}',
           |  '${CsvFile.urify(csvB)}',
           |  '${CsvFile.urify(csvC)}'] as csv
           |LOAD CSV FROM csv AS line
           |CREATE (n:Artist {name: line[1], year: toInteger(line[2])})
           |RETURN filename() AS file, linenumber() AS line, n.name AS artist, n.year AS born""".stripMargin, mapFilePath, ResultAssertions(r => {
          r.columnAs("line").toList should equal(List(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4))
        })) {
        p("The filename and linenumber used for each node created is shown.")
        resultTable()
      }
    }
  }.build()

}
