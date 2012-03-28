redo-ifchange Data/Money.lhs
pandoc -f markdown+lhs -t markdown -i Data/Money.lhs | \
  sed "s/~~~~/\`\`\`/g" | \
  sed "s/\ {\.sourceCode\ \.literate\ \.haskell}/haskell/g"
