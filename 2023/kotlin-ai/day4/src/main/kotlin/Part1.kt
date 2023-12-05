import java.io.File

fun parseNumbers(input: String) =
    input.split(" ").filter { !it.equals("") }.map { it.toInt() }.toSet()
fun parseCards(input: List<String>): List<Pair<Set<Int>, Set<Int>>> =
    input.map { line ->
        val (_, winning, mine) = line.split(Regex("[:|]"))
        Pair(parseNumbers(winning), parseNumbers(mine))
    }

fun calculateWinners(input: List<Pair<Set<Int>, Set<Int>>>) =
    input.map { (winning, mine) -> winning.intersect(mine).size }

fun calculatePoints(input: List<Int>) =
    input.filter { it > 0 }
         .map { 1 shl ( it - 1 )}

fun main() {
    val input = File("input.txt").readText()
    val lines = input.lines()
    println(calculatePoints(calculateWinners(parseCards(lines))).sum())
}