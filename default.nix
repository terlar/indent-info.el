{ elpaBuild, writeText }:

elpaBuild rec {
  pname   = "indent-info";
  ename   = pname;
  version = "0";
  src = ./indent-info.el;
}
