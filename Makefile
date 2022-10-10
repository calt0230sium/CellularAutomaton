default:
	ghc --make -o build/Main.exe src/Line.hs src/Plan.hs src/Main.hs -no-keep-hi-files -no-keep-o-files
