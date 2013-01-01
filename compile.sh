pushd public/Support 1>/dev/null
rm *.js 2>/dev/null
elm --only-js *.elm
mv *.js ../../resources
popd 1>/dev/null
cd server
ghc --make -O2 -threaded -hidir ghc_output -odir ghc_output Server.hs
mv Server ../elm-server
cd ..
