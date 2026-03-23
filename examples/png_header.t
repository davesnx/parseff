PNG Header Parser

Parse a valid PNG file:

  $ ./png_header.exe test.png
  test.png:
    2x3, 8-bit RGB

Error on non-PNG input:

  $ ./png_header.exe bad.bin
  bad.bin: error at byte 8: PNG signature

Error on truncated file (only the 8-byte signature, no chunks):

  $ ./png_header.exe truncated.bin
  truncated.bin: unexpected end of input at byte 8
