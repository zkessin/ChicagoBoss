#!/bin/bash


PLT=plt/cb.plt
echo $PLT

if [ ! -f $PLT ]; then
   dialyzer  --build_plt --apps kernel stdlib mnesia  inets ssl crypto \
       erts public_key runtime_tools compiler asn1 hipe gs\
       syntax_tools edoc xmerl \
       --statistics\
       --output_plt $PLT
  
   echo "********************************************************************************"
   dialyzer --add_to_plt deps/*/ebin						--plt $PLT
   echo "********************************************************************************"
   echo ""
fi
