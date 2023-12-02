import java.io.File

typealias Cubes = Map<String, Int>

data class Game(val id: Int, val cubes: List<Cubes>)

fun parseColors(colorString: String): Cubes =
    colorString.split(",")
        .map(String::trim)
        .map { it.split(" ") }
        .associateBy({ it[1] }, { it[0].toInt() })

fun parseAttempts(input: String): List<String> =
    input.split(";")

fun parseLine(input: String): List<Cubes> =
    parseAttempts(input).map(::parseColors)

fun parseGame(input: String): Game {
    val (name, cubes) = input.split(":")
    val (_, id) = name.split(" ")
    return Game(id.toInt(), parseLine(cubes))
}

fun combineMaps(map1: Cubes, map2: Cubes): Cubes =
    (map1.keys + map2.keys).distinct()
        .associateWith { maxOf(map1[it] ?: 0, map2[it] ?: 0) }

fun compareMaps(map1: Cubes, map2: Cubes): Boolean {
    return map1.all { (key, value) -> value >= (map2[key] ?: 0) }
}

fun parseGames(inputs: List<String>): List<Game> =
    inputs.map(::parseGame)

fun filterGames(games: List<Game>, bag: Cubes): List<Game> =
    games.filter { compareMaps(bag, it.cubes.reduce(::combineMaps)) }

fun sumGames(games: List<Game>): Int =
    games.map(Game::id).sum()

fun main() {
    val input = File("input.txt").readText()
    val bag = mapOf("red" to 12, "green" to 13, "blue" to 14)
    val games = parseGames(input.lines())
    val sum = sumGames(filterGames(games, bag))
    println(sum)
}