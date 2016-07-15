
.PHONY: default clean

files     := Client_Classes FindColor Input_Diag Window DTM TMWindow client_classes_dependencies

default: $(files:=.png)
	
clean:
	rm -f *.png

%.png: %.dot
	dot $< -Tpng > $@

