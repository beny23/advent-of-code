import java.io.File

val digitWords = mapOf(
    "zero" to "0",
    "one" to "1",
    "two" to "2",
    "three" to "3",
    "four" to "4",
    "five" to "5",
    "six" to "6",
    "seven" to "7",
    "eight" to "8",
    "nine" to "9"
)

val reverseDigitWords = digitWords.mapKeys { it.key.reversed() }

fun replaceFirstWord(input: String, words: Map<String, String>): String {
    val regex = words.keys.joinToString("|").toRegex()
    val match = regex.find(input)
    return if (match != null) {
        input.substring(0, match.range.first) + words[match.value] + input.substring(match.range.last)
    } else {
        input
    }
}

fun firstDigit(input: String, words: Map<String, String> = digitWords): Int =
    replaceFirstWord(input, words).toCharArray().filter(Char::isDigit).map(Char::digitToInt).first()

fun parseLine(input: String): Int {
    val value1 = firstDigit(input, digitWords)
    val value2 = firstDigit(input.reversed(), reverseDigitWords)
    return value1 * 10 + value2
}

fun main() {
    val input = File("input.txt").readText()

    val sum = input.lines().map(::parseLine).sum()

    println(sum)
}