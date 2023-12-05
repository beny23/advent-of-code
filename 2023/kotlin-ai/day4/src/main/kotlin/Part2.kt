import java.io.File
import java.util.Stack

fun calculateCumulativeWins(input: List<String>): List<Int> {
    val cards = parseCards(input)
    val winners = calculateWinners(cards)

    val numberOfCards = MutableList<Int>(winners.size) { 1 }

    winners.forEachIndexed { i, e ->
        val card = numberOfCards[i]
        for (n in i+1..i+e) {
            numberOfCards[n] += card
        }
    }

    return numberOfCards // Return the cumulative wins
}
fun main() {
    val input = File("input.txt").readText()
    val lines = input.lines()
    val wins = calculateCumulativeWins(lines)
    println(wins)
    println(wins.sum())
}