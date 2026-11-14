HTTP/1.1 heads, pipelined requests and chunked bodies

Three pipelined requests on one input: a plain GET, a form body of known
length parsed with within, and a chunked body:

  $ printf 'GET /index.html HTTP/1.1\r\nHost: example.com\r\nAccept: */*  \r\n\r\nPOST /form HTTP/1.1\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 11\r\n\r\na=1&b=2&c=3PUT /up HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nhello\r\n6;ext=1\r\n world\r\n0\r\nX-Trailer: t\r\n\r\n' > pipelined.txt
  $ ./main.exe requests < pipelined.txt
  GET /index.html HTTP/1.1
    Host: example.com
    Accept: */*
  POST /form HTTP/1.1
    Content-Type: application/x-www-form-urlencoded
    Content-Length: 11
    form: a = 1
    form: b = 2
    form: c = 3
  PUT /up HTTP/1.1
    Transfer-Encoding: chunked
    body: "hello world"

The same input read one byte at a time with a window of 64 bytes:

  $ ./main.exe --chunk 1 --window 64 requests < pipelined.txt
  GET /index.html HTTP/1.1
    Host: example.com
    Accept: */*
  POST /form HTTP/1.1
    Content-Type: application/x-www-form-urlencoded
    Content-Length: 11
    form: a = 1
    form: b = 2
    form: c = 3
  PUT /up HTTP/1.1
    Transfer-Encoding: chunked
    body: "hello world"

A response head with a body:

  $ printf 'HTTP/1.1 200 OK\r\nContent-Length: 2\r\n\r\nhi' | ./main.exe response
  HTTP/1.1 200 OK
    Content-Length: 2
    body: "hi"

A chunked body with a trailer:

  $ printf '5\r\nhello\r\n6\r\n world\r\n0\r\nExpires: never\r\n\r\n' | ./main.exe chunked
  body: "hello world"
    Expires: never

A header value over the limit of 8190 bytes:

  $ long=$(head -c 9000 /dev/zero | tr '\0' a)
  $ printf "GET / HTTP/1.1\r\nX-Long: $long\r\n\r\n" | ./main.exe requests
  error 431 Request Header Fields Too Large at byte 24: limit of 8190 exceeded

The same with a window smaller than the limit:

  $ printf "GET / HTTP/1.1\r\nX-Long: $long\r\n\r\n" | ./main.exe --chunk 7 --window 1024 requests
  error 431 Request Header Fields Too Large at byte 24: backtrack window of 1024 bytes exceeded

A request target over the limit:

  $ printf "GET /$long HTTP/1.1\r\n\r\n" | ./main.exe requests
  error 414 URI Too Long at byte 4: limit of 8190 exceeded

More than 100 headers:

  $ (printf 'GET / HTTP/1.1\r\n'; for i in $(seq 101); do printf "H$i: v\r\n"; done; printf '\r\n') | ./main.exe requests
  error 431 Request Header Fields Too Large at byte 808: limit of 100 exceeded

A head cut before its end, read whole and one byte at a time:

  $ printf 'GET / HTTP/1.1\r\nHost: a\r\n' | ./main.exe requests
  error 400 Bad Request at byte 25: expected field name or "\r\n", found end of input
  $ printf 'GET / HTTP/1.1\r\nHost' | ./main.exe --chunk 1 requests
  error 400 Bad Request at byte 20: expected ':', found end of input

A space in a field name:

  $ printf 'GET / HTTP/1.1\r\nBad Header: x\r\n\r\n' | ./main.exe requests
  error 400 Bad Request at byte 19: expected ':', found ' '

A form body shorter than its Content-Length:

  $ printf 'POST / HTTP/1.1\r\nContent-Type: application/x-www-form-urlencoded\r\nContent-Length: 20\r\n\r\na=1' | ./main.exe requests
  error 400 Bad Request at byte 91: expected 17 bytes or '&', found end of input

A chunk size that does not match the data, a chunk size that is not
hexadecimal, a chunk size with too many digits, and a chunk cut short:

  $ printf '5\r\nhello world\r\n0\r\n\r\n' | ./main.exe chunked
  error 400 Bad Request at byte 8: expected "\r\n", found ' '
  $ printf 'zz\r\nhello\r\n0\r\n\r\n' | ./main.exe chunked
  error 400 Bad Request at byte 0: expected chunk size, found 'zz'
  $ printf '1234567890abcdef0\r\nx\r\n' | ./main.exe chunked
  error 400 Bad Request at byte 0: limit of 15 exceeded
  $ printf '5\r\nhel' | ./main.exe chunked
  error 400 Bad Request at byte 3: expected 5 bytes, found only 3 (end of input)

A status code that is not a number:

  $ printf 'HTTP/1.1 20x OK\r\n\r\n' | ./main.exe response
  error 400 Bad Request at byte 11: expected digit, found 'x'

Chunk extensions must have the form ;name or ;name=value, with a token or a
quoted string as the value. Other bytes before the CRLF, and a bare LF, are
errors:

  $ printf '5;name="a \\"q\\"" ; x = 1\r\nhello\r\n0\r\n\r\n' | ./main.exe chunked
  body: "hello"
  $ printf '5garbage\nmore\r\nhello\r\n0\r\n\r\n' | ./main.exe chunked
  error 400 Bad Request at byte 1: expected ';' or "\r\n", found 'garbage'
  $ printf '5;=x\r\nhello\r\n0\r\n\r\n' | ./main.exe chunked
  error 400 Bad Request at byte 2: expected extension name, found '='
  $ printf '5\nhello\r\n0\r\n\r\n' | ./main.exe chunked
  error 400 Bad Request at byte 1: expected ';' or "\r\n", found '\n'

Framing headers (RFC 9112 section 6.3, RFC 9110 section 8.6). Two
Content-Length values that differ are an error; the same value twice is
accepted:

  $ printf 'POST / HTTP/1.1\r\nContent-Length: 3\r\nContent-Length: 10\r\n\r\nabc' | ./main.exe requests
  error 400 Bad Request at byte 58: conflicting Content-Length values
  $ printf 'POST / HTTP/1.1\r\nContent-Length: 3\r\nContent-Length: 3\r\n\r\nabc' | ./main.exe requests
  POST / HTTP/1.1
    Content-Length: 3
    Content-Length: 3
    body: "abc"

Transfer-Encoding with Content-Length, chunked twice, chunked not last, and a
coding that the example does not implement:

  $ printf 'POST / HTTP/1.1\r\nTransfer-Encoding: chunked\r\nContent-Length: 3\r\n\r\n3\r\nabc\r\n0\r\n\r\n' | ./main.exe requests
  error 400 Bad Request at byte 66: Transfer-Encoding and Content-Length together
  $ printf 'POST / HTTP/1.1\r\nTransfer-Encoding: chunked\r\nTransfer-Encoding: chunked\r\n\r\n3\r\nabc\r\n0\r\n\r\n' | ./main.exe requests
  error 400 Bad Request at byte 75: chunked must be the final transfer coding, once
  $ printf 'POST / HTTP/1.1\r\nTransfer-Encoding: chunked, gzip\r\n\r\n' | ./main.exe requests
  error 400 Bad Request at byte 53: chunked must be the final transfer coding, once
  $ printf 'POST / HTTP/1.1\r\nTransfer-Encoding: gzip, chunked\r\n\r\n3\r\nabc\r\n0\r\n\r\n' | ./main.exe requests
  error 501 Not Implemented at byte 53: unsupported transfer coding gzip

A method longer than 32 bytes:

  $ (head -c 200000 /dev/zero | tr '\0' G; printf ' / HTTP/1.1\r\n\r\n') | ./main.exe requests
  error 501 Not Implemented at byte 0: limit of 32 exceeded
