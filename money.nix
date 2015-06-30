{ mkDerivation, base, data-default, Decimal, stdenv }:
mkDerivation {
  pname = "money";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ base data-default Decimal ];
  homepage = "https://github.com/jb55/money";
  description = "Money type";
  license = stdenv.lib.licenses.bsd3;
}
