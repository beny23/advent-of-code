import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testParseCards() {
        val input = listOf(
            "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53",
            "Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19")
        val expected = listOf(
            Pair(setOf(41, 48, 83, 86, 17), setOf(83, 86, 6, 31, 17, 9, 48, 53)),
            Pair(setOf(13, 32, 20, 16, 61), setOf(61, 30, 68, 82, 17, 32, 24, 19))
        )
        assertEquals(expected, parseCards(input))
    }
}
