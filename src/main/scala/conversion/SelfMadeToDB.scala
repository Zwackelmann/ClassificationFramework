package conversion

import java.io.InputStreamReader
import java.io.FileInputStream
import java.lang.RuntimeException
import scala.collection.mutable.ListBuffer
import common.WriteFile._
import common.ErrorLog
import common.NoticeEvery
import model._
import parser.SelfMadeParser

object SelfMadeToDB {
    import Author.unifyName
    
    def main(args: Array[String]) {
        var noticeEvery = new NoticeEvery(10000)
        val errorLog = new ErrorLog[List[String]](noticeEveryXErrors = Some(100), filename = Some("log/selfMadeToDBErrors.log")) {
            override def tToString(t: List[String]) = t.mkString("\n")
        }
        val parser = new SelfMadeParser("../Common/data/raw/deliver-math.txt")

        def read() {
            val lines = parser.nextLines
            
            try {
                val paper = parser.readPaper(lines)
                DB.addPaper(paper)
            } catch {
                case re: RuntimeException => 
                    errorLog.log(lines.toList, re)
            }
            
            noticeEvery.count
            
            if(noticeEvery.counter % 10000 == 0) {
                DB.commit
            }
            
            if(parser.hasNext) {
                read()
            }
        }
        
        read()
        DB.commit
        
        errorLog.logErrors
    }
}



object DB extends common.DB(
        // dbms = "mysql"
        dbms = "postgres",
        host = "localhost",
        port = "default",
        // db = "dm_master",
        // user = "barthel",
        user = "postgres",
        // pw = "eR3NJUapbP9A5PtF"
        pw = "0118999"
        
) {
    connection.setAutoCommit(false)
    
    val insertPaperStmt = connection.prepareStatement("INSERT INTO paper(an1, an2, title, abstract) VALUES (?, ?, ?, ?)")
    val insertAuthorStmt = connection.prepareStatement("INSERT INTO author(paper_an1, paper_an2, author) VALUES (?, ?, ?)")
    val insertClassStmt = connection.prepareStatement("INSERT INTO class_raw(paper_an1, paper_an2, class) VALUES (?, ?, ?)")
    val insertTermStmt = connection.prepareStatement("INSERT INTO term(paper_an1, paper_an2, term) VALUES (?, ?, ?)")
    val insertSourceStmt = connection.prepareStatement(
        "INSERT INTO source(" +
            "paper_an1, " +
            "paper_an2, " +
            "simple_source, " +
            "journal, " +
            "volume, " +
            "nr_from, " +
            "nr_to, " +
            "page_from, " +
            "page_to, " +
            "year) " +
        "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
    )
    val insertJsonStmt = connection.prepareStatement("INSERT INTO json_paper(paper_an1, paper_an2, json) VALUES (?, ?, ?)")
    
    def addPaper(paper: Paper) {
        insertPaperStmt.setInt(1, paper.an._1)
        insertPaperStmt.setInt(2, paper.an._2)
        insertPaperStmt.setString(3, paper.title)
        insertPaperStmt.setString(4, paper.abstractText)
        
        insertPaperStmt.addBatch()
        
        insertJsonStmt.setInt(1, paper.an._1)
        insertJsonStmt.setInt(2, paper.an._2)
        insertJsonStmt.setString(3, paper.toJson.toString)
        insertJsonStmt.addBatch()
        
        for(author <- paper.authors) {
            insertAuthorStmt.setInt(1, paper.an._1)
            insertAuthorStmt.setInt(2, paper.an._2)
            insertAuthorStmt.setString(3, author.unifiedName)
            insertAuthorStmt.addBatch()
        }
        
        for(classCode <- paper.mscClasses) {
            insertClassStmt.setInt(1, paper.an._1)
            insertClassStmt.setInt(2, paper.an._2)
            insertClassStmt.setString(3, classCode)
            insertClassStmt.addBatch()
        }
        
        for(term <- paper.terms) {
            insertTermStmt.setInt(1, paper.an._1)
            insertTermStmt.setInt(2, paper.an._2)
            insertTermStmt.setString(3, if(term.length > 255) term.substring(0, 255) else term)
            insertTermStmt.addBatch()
        }
        
        for(source <- paper.sources) {
            insertSourceStmt.setInt(1, paper.an._1)
            insertSourceStmt.setInt(2, paper.an._2)
            source match {
                case ds: DetailledSource => {
                    insertSourceStmt.setObject(3, null)
                    insertSourceStmt.setString(4, (if(ds.journal.length() > 255) ds.journal.substring(0, 255) else ds.journal))
                    insertSourceStmt.setInt(5, ds.volume)
                    insertSourceStmt.setInt(6, ds.nrFrom)
                    insertSourceStmt.setInt(7, ds.nrTo)
                    insertSourceStmt.setInt(8, ds.pageFrom)
                    insertSourceStmt.setInt(9, ds.pageTo)
                    insertSourceStmt.setInt(10, ds.pageTo)
                }
                case os: OtherSource => {
                    insertSourceStmt.setString(3, (if(os.src.length() > 255) os.src.substring(0, 255) else os.src))
                    insertSourceStmt.setObject(4, null)
                    insertSourceStmt.setObject(5, null)
                    insertSourceStmt.setObject(6, null)
                    insertSourceStmt.setObject(7, null)
                    insertSourceStmt.setObject(8, null)
                    insertSourceStmt.setObject(9, null)
                    insertSourceStmt.setObject(10, null)
                }
            }
            insertSourceStmt.addBatch()
        }
    }
    
    def commit {
        println("commit")
        insertPaperStmt.executeBatch()
        insertAuthorStmt.executeBatch()
        insertClassStmt.executeBatch()
        insertTermStmt.executeBatch()
        insertSourceStmt.executeBatch()
        insertJsonStmt.executeBatch()
        connection.commit
    }
}