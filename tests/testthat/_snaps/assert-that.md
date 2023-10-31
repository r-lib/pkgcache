# assert_that

    Code
      assert_that(is.integer("a"))
    Condition
      Error:
      ! is.integer("a") is not TRUE

---

    Code
      assert_that(is.integer(1L[[2]]))
    Condition
      Error:
      ! subscript out of bounds

# bad assertions

    Code
      assert_that(1:10)
    Condition
      Error:
      ! assert_that: assertion must return a logical value

---

    Code
      assert_that(c(TRUE, NA))
    Condition
      Error:
      ! assert_that: missing values present in assertion

---

    Code
      assert_that(c(TRUE, TRUE))
    Condition
      Error:
      ! assert_that: length of assertion is not 1

# call is deparsed correctly

    Code
      assert_that(is_integer(1.1))
    Condition
      Error:
      ! is_integer(x = 1.1) is not TRUE

---

    Code
      assert_that(is_integer(1.00001 * 1.00001 * 1.00001 * 1.00001 * 1.00001 *
      1.00001 * 1.00001 * 1.00001 * 1.00001 * 1.00001))
    Condition
      Error:
      ! is_integer(x = 1.00001 * 1.00001 * 1.00001 * 1.00001 * 1.00001 * ... is not TRUE

# custom failure message

    Code
      assert_that(is_count("nope"))
    Condition
      Error:
      ! "nope" is not a count, a positive integer scalar

