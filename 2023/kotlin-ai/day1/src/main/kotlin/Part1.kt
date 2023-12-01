import java.io.File

fun main() {
    val input = File("input.txt").readText()

    var sum = 0
    for (line in input.lines()) {
        val value = line.toCharArray().filter(Char::isDigit).map(Char::digitToInt)
        sum += value[0] * 10 + value[value.size - 1]
    }

    println(sum)
}