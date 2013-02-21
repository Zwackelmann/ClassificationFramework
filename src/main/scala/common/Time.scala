package common

object Time {
    def apply[T](times: Int = 1, promt: String = "Time measurement")(fun: => T) = {
        val range = 0 until times-1
        val time = System.currentTimeMillis()
        for(i <- range) {
            fun
        }
        val t = fun
        val time2 = System.currentTimeMillis();
        
        println(promt + ": " + (time2 - time))
        t
    }
}

object ReturnTime {
    def apply[T](fun: => T) = {
        val time = System.currentTimeMillis()
        val t = fun
        val time2 = System.currentTimeMillis();
        
        (t, (time2 - time))
    }
}