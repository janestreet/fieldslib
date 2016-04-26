#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"fieldslib"
  [ oasis_lib "fieldslib"
  ; file "META" ~section:"lib"
  ]
