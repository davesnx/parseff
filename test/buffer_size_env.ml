let buffer_size =
  Option.map int_of_string (Sys.getenv_opt "PARSEFF_TEST_BUFFER_SIZE")
