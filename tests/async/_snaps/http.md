# http_status

    Code
      http_status(0)
    Condition
      Error:
      ! Unknown http status code: 0

# http_post form

    Code
      obj$files
    Output
      $baz
      $baz$filename
      [1] "mrfile"
      
      $baz$value
      [1] "data:application/octet-stream;base64,MDEyMzQ1Njc4OQ=="
      
      

---

    Code
      obj$form
    Output
      $foo
      [1] "bar"
      

