  $ cat >test <<EOF
  > NNCB
  > 
  > CH -> B
  > HH -> N
  > CB -> H
  > NH -> C
  > HB -> C
  > HC -> B
  > HN -> C
  > NN -> C
  > BH -> H
  > NC -> B
  > NB -> B
  > BN -> B
  > BB -> N
  > BC -> B
  > CC -> N
  > CN -> C 
  > EOF

  $ ./day14.exe test
  After 10 steps, the quantity of the least common element subtracted from the quantity of the most common element is 1588
  After 40 steps, the quantity of the least common element subtracted from the quantity of the most common element is 2188189693529
