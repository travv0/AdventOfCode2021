  $ cat >test <<EOF
  > 8A004A801A8002F478
  > EOF

  $ ./day16.exe test
  The version sum of the package hierarchy is 16

  $ cat >test <<EOF
  > 620080001611562C8802118E34
  > EOF

  $ ./day16.exe test
  The version sum of the package hierarchy is 12

  $ cat >test <<EOF
  > C0015000016115A2E0802F182340
  > EOF

  $ ./day16.exe test
  The version sum of the package hierarchy is 23

  $ cat >test <<EOF
  > A0016C880162017C3686B18A3D4780
  > EOF

  $ ./day16.exe test
  The version sum of the package hierarchy is 31
