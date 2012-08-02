package common

class NoticeEvery(val x: Int, val name: String = "Counter") {
    var counter = 0
    def count {
        counter = counter + 1
        if(counter % x == 0) {
            println(name + ": " + counter)
        }
    }
}