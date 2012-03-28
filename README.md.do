redo-ifchange Money.lhs
pandoc -f markdown+lhs -t markdown -i Money.lhs | \
  sed "s/\.sourceCode\ \.literate\ //g"
