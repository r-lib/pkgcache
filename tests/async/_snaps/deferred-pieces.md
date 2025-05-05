# def__make_parent_*

    Code
      def__make_parent_resolve(f)
    Condition
      Error in `as.function.default()`:
      ! list argument expected

---

    Code
      def__make_parent_reject(f)
    Condition
      Error in `as.function.default()`:
      ! list argument expected

---

    Code
      def__make_parent_resolve(f)
    Condition
      Error in `def__make_parent_resolve()`:
      ! Invalid parent_resolve callback

---

    Code
      def__make_parent_reject(f)
    Condition
      Error in `def__make_parent_reject()`:
      ! Invalid parent_reject callback

# def__make_parent_resolve

    Code
      r1("foobar", function(x) {
        res <<- "resolve"
        val <<- x
      })
    Condition
      Error in `r1()`:
      ! foobar

