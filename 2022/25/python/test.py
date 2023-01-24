import unittest
import snafu

DECIMALS = [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20, 2022, 12345, 314159265, 1747, 906, 198, 11, 201, 31, 1257, 32, 353, 107, 7, 3, 37, 4890 ]
SNAFUS = [ "1", "2", "1=", "1-", "10", "11", "12", "2=", "2-", "20", "1=0","1-0", "1=11-2", "1-0---0", "1121-1110-1=0", "1=-0-2", "12111", "2=0=", "21", "2=01", "111", "20012", "112", "1=-1=", "1-12", "12", "1=", "122", "2=-1=0" ]

class SnafuTest(unittest.TestCase):
    def test_to(self):
        for i in range(len(DECIMALS)):
            result = snafu.to_snafu(DECIMALS[i])
            self.assertEqual(result, SNAFUS[i])

    def test_from(self):
        for i in range(len(DECIMALS)):
            result = snafu.from_snafu(SNAFUS[i])
            self.assertEqual(result, DECIMALS[i])

unittest.main()
