import org.junit.jupiter.api.Test
import kotlin.test.assertEquals

class Tests {
    @Test
    fun testReplaceDigits() {
        assertEquals(2, firstDigit("two"))
        assertEquals(2, firstDigit("two1"))
        assertEquals(2, firstDigit("two1nine"))
        assertEquals(2, firstDigit("two1nine8seventhree4fivesix"))
        assertEquals(2, firstDigit("2two2two2"))
        assertEquals(2, firstDigit("twone3"))
        assertEquals(2, firstDigit("twone3twone"))
    }

    @Test
    fun testParseLine() {
        assertEquals(29, parseLine("two1nine"))
        assertEquals(83, parseLine("eightwothree"))
        assertEquals(13, parseLine("abcone2threexyz"))
        assertEquals(24, parseLine("xtwone3four"))
        assertEquals(42, parseLine("4nineeightseven2"))
        assertEquals(14, parseLine("zoneight234"))
        assertEquals(76, parseLine("7pqrstsixteen"))
    }

}
