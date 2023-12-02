import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun `should parse colours correctly`() {
        val input = "5 blue, 4 red, 13 green"
        val expectedResult = mapOf("blue" to 5, "red" to 4, "green" to 13)
        assertEquals(expectedResult, parseColors(input))
    }

    @Test
    fun `should return correct values for when not all colours are present`() {
        val input = "4 red, 13 green"
        val expectedOutput = mapOf("red" to 4, "green" to 13)
        assertEquals(expectedOutput, parseColors(input))
    }

    @Test
    fun `should split string delimited by semicolons`() {
        val input = "apple;banana;cherry"
        val expectedOutput = listOf("apple", "banana", "cherry")
        assertEquals(expectedOutput, parseAttempts(input))
    }

    @Test
    fun testParseLine() {
        val input = "3 red, 4 blue; 4 red, 5 green; 6 blue, 3 green"
        val expectedOutput = listOf(
            mapOf("red" to 3, "blue" to 4),
            mapOf("red" to 4, "green" to 5),
            mapOf("blue" to 6, "green" to 3)
        )

        val actualOutput = parseLine(input)

        assertEquals(expectedOutput, actualOutput)
    }

    @Test
    fun testCombineMaps() {
        val map1 = mapOf("a" to 1, "b" to 2)
        val map2 = mapOf("a" to 3, "c" to 4)

        val expectedOutput = mapOf("a" to 3, "b" to 2, "c" to 4)

        assertEquals(expectedOutput, combineMaps(map1, map2))
    }

    @Test
    fun testCompareMapsTrue() {
        val map1 = mapOf("a" to 4, "b" to 6)
        val map2 = mapOf("a" to 4, "c" to 5)
        assertEquals(true, compareMaps(map1, map2))
    }

    @Test
    fun testCompareMapsFalse() {
        val map1 = mapOf("a" to 3, "b" to 6)
        val map2 = mapOf("a" to 4, "c" to 5)
        assertEquals(false, compareMaps(map1, map2))
    }

    @Test
    fun testParseGame() {
        val input = "Game 5: 6 red, 1 blue, 3 green; 2 red, 2 green"
        val expectedGameId = 5
        val expectedCubes = listOf(mapOf("red" to 6, "blue" to 1, "green" to 3), mapOf("red" to 2, "green" to 2))
        val game = parseGame(input)
        assertEquals(game.id, expectedGameId)
        assertEquals(game.cubes, expectedCubes)
    }
}
