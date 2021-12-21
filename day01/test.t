  $ cat >test <<EOF
  > 199
  > 200
  > 208
  > 210
  > 200
  > 207
  > 240
  > 269
  > 260
  > 263
  > EOF

  $ ./day1.exe test
  Number of times depth increases: 7
  Number of times sum in sliding window increases: 5
