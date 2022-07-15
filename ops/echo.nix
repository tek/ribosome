''
mag="\e[35m"
green="\e[32m"
msg="\e[34m"
emph() {
  echo -e "\e[33m$*$msg"
}
log() {
  echo -e "\e[1m$green>>=\e[0m \e[34m$*\e[0m" >&2
}
die() {
  log "\e[31mERROR: $*"
  exit 1
}
''
