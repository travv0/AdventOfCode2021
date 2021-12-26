  $ cat >test <<EOF
  > start-A
  > start-b
  > A-c
  > A-b
  > b-d
  > A-end
  > b-end
  > EOF

  $ ./day12.exe test
  There are 10 paths through the cave system if each small cave is visited once.
  There are 36 paths through the cave system if one small cave can be visited twice.

  $ cat >test <<EOF
  > dc-end
  > HN-start
  > start-kj
  > dc-start
  > dc-HN
  > LN-dc
  > HN-end
  > kj-sa
  > kj-HN
  > kj-dc
  > EOF

  $ ./day12.exe test
  There are 19 paths through the cave system if each small cave is visited once.
  There are 103 paths through the cave system if one small cave can be visited twice.
