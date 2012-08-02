package common

import java.sql.DriverManager
import java.sql.SQLException


class DB(dbms: String, host: String, port: String = "", db: String = "", user: String, pw: String) {
    if(!List("mysql", "postgres").contains(dbms)) {
        throw new IllegalArgumentException("Illegal db name")
    }
    
    def _port = if(port == "default") {
        if(dbms == "mysql") "3306"
        else if(dbms == "postgres") "5432"
        else throw new RuntimeException("invalid dbmsname")
    } else {
        port
    }
    
    def prefix = if(dbms == "mysql") "mysql"
        else if(dbms == "postgres") "postgresql"
        else throw new RuntimeException("invalid dbmsname")
    
    lazy val connection = getConnection
    
    def getConnection = {
        try {
            if(dbms == "mysql") {
            	Class.forName("com.mysql.jdbc.Driver")
            } else if(dbms == "postgres") {
            	Class.forName("org.postgresql.Driver")
            } else {
                throw new RuntimeException("invalid dbmsname")
            }
        } catch {
            case cnfe: ClassNotFoundException =>
                println("Couldn't find the driver!")
                System.exit(1)
        }

        val c = try {
            DriverManager.getConnection("jdbc:" + prefix + "://" + host + (if(port != "") ":" + _port else "") + (if(db != "") "/" + db else ""), user, pw)
        } catch {
            case sqle: SQLException =>
                System.out.println("Couldn't connect")
                sqle.printStackTrace()
                System.exit(1)
                null
        }
        
        c
    }
}