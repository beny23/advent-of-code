import java.io.File

typealias Pos = Pair<Int, Int>
fun findNumbers(input: List<String>, includePositions: Set<Pos>): List<Int> {
    val regex = "\\d+".toRegex()
    val results = mutableListOf<Int>()

    for ((y, row) in input.withIndex()) {
        var matchResult: MatchResult? = regex.find(row)

        while (matchResult != null) {
            val number = matchResult.value
            for (x in matchResult.range) {
                if (includePositions.contains(Pair(x, y))) {
                    results.add(number.toInt())
                    break
                }
            }

            matchResult = matchResult.next()
        }
    }

    return results
}

fun findNonDigitPositions(input: List<String>): Set<Pos> {
    val results = mutableSetOf<Pos>()

    for ((y, row) in input.withIndex()) {
        for ((x, character) in row.withIndex()) {
            if (character !in '0'..'9' && character != '.') {
                for (i in -1..1) {
                    for (j in -1..1) {
                        results.add(Pair(x + i, y + j))
                    }
                }
            }
        }
    }

    return results
}


fun main() {
    val input = File("input.txt").readText()
    val lines = input.lines()
    val covered = findNonDigitPositions(lines)
    println(findNumbers(lines, covered).sum())
}