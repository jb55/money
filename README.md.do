redo-ifchange Money.lhs
pandoc -f markdown+lhs -t markdown -i Money.lhs | \
  sed "s/~~~~/\`\`\`/g" | \
  sed "s/\ {\.sourceCode\ \.literate\ \.haskell}/haskell/g"
