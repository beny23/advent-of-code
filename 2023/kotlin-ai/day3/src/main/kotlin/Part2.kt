import java.io.File

data class Result(val value: Int, val range: IntRange)

fun findNumbersInString(input: List<String>) =
    input.mapIndexed { y, str ->
        y to Regex("\\d+").findAll(str).map { match ->
            Result(match.value.toInt(), match.range)
        }.toList()
    }.toMap()

fun getResultsAtCoordinate(resultsMap: Map<Int, List<Result>>, xRange: IntRange, yRange: IntRange): List<Result> =
    resultsMap
        .filterKeys { y -> yRange.contains(y) }
        .values
        .flatten()
        .filter { result -> result.range.intersect(xRange).isNotEmpty() }

fun findAsterisks(input: List<String>): List<Pair<Int, Int>> =
    input.flatMapIndexed { y, str ->
        str.mapIndexedNotNull { x, char -> if (char == '*') y to x else null }
    }

fun findAsteriskWithResults(asteriskCoordinates: List<Pair<Int, Int>>, resultsMap: Map<Int, List<Result>>) =
    asteriskCoordinates.map { (y, x) ->
        val results = getResultsAtCoordinate(resultsMap, (x-1..x+1), (y-1..y+1))
        if (results.size == 2) {
            results.map(Result::value).reduce { a, b -> a * b }
        } else {
            0
        }
    }


fun main() {
    val input = File("input.txt").readText()
    println(input)
    val lines = input.lines()
    val asteriskCoordinates = findAsterisks(lines)
    println(asteriskCoordinates)
    val resultsMap = findNumbersInString(lines)
    println(resultsMap)
    println(findAsteriskWithResults(asteriskCoordinates, resultsMap).sum())
}