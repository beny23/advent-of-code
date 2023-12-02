import java.io.File

fun minimum(game: Game): Cubes =
    game.cubes.reduce(::combineMaps)

fun power(cubes: Cubes): Int =
    cubes.values.reduce { a, b -> a * b }

fun main() {
    val input = File("input.txt").readText()
    val games = parseGames(input.lines())
    val sum = games.map(::minimum).map(::power).sum()
    println(sum)
}