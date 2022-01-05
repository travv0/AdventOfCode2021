  $ cat >test <<EOF
  > 0,9 -> 5,9
  > 8,0 -> 0,8
  > 9,4 -> 3,4
  > 2,2 -> 2,1
  > 7,0 -> 7,4
  > 6,4 -> 2,0
  > 0,9 -> 2,9
  > 3,4 -> 1,4
  > 0,0 -> 8,8
  > 5,5 -> 8,2
  > EOF

  $ ./day5.exe test
  Number of points where at least two horizontal or vertical lines overlap: 5
  Number of points where at least two lines overlap: 12

