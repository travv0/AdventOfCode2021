  $ cat >test <<EOF
  > forward 5
  > down 5
  > forward 8
  > up 3
  > down 8
  > forward 2
  > EOF

  $ ./day2.exe test
  Final horizontal position multiplied by final depth for part 1: 150
  Final horizontal position multiplied by final depth for part 2: 900
