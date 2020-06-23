from doct import square
import unittest

class ProductTestCase(unittest.TestCase):
    def test_integers(self):
        for x in range(-10, 10):
            self.assertEqual(square(x), x*x, 'Integer psquare failed')
    def test_floats(self):
        for x in range(-10, 10):
            self.assertEqual(square(x/10), x/10*(x/10), 'Float psquare failed')

if __name__ == "__main__":
    unittest.main()

# $ python3 unitt.py -v
# test_floats (__main__.ProductTestCase) ... ok
# test_integers (__main__.ProductTestCase) ... ok
# 
# ---------------------------------------------------------------------
# Ran 2 tests in 0.000s

# 失败
# FF
# ======================================================================
# FAIL: test_floats (__main__.ProductTestCase)
# ----------------------------------------------------------------------
# Traceback (most recent call last):
#   File "unitt.py", line 10, in test_floats
#     self.assertEqual(square(x/10), x/10*(x/10), 'Float psquare failed')
# AssertionError: -2.0 != 1.0 : Float psquare failed
# 
# ======================================================================
# FAIL: test_integers (__main__.ProductTestCase)
# ----------------------------------------------------------------------
# Traceback (most recent call last):
#   File "unitt.py", line 7, in test_integers
#     self.assertEqual(square(x), x*x, 'Integer psquare failed')
# AssertionError: -20 != 100 : Integer psquare failed
# 
# ----------------------------------------------------------------------
# Ran 2 tests in 0.001s
# 
# FAILED (failures=2)
