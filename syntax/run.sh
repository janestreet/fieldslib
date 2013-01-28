#!/usr/bin/env bash
set -e -u
jomake --command=camlp4orf -I "$(hg root)/lib" pa_type_conv.cmo pa_fields_conv.cmo $@
