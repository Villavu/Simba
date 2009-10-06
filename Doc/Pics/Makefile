
.PHONY: default clean

files := Client_Classes FindColor Input_Diag Window Window_Types
build = dot $(1).dot -Tpng > $(1).png

default: dotit
	
clean:
	rm -f *.png

dotit: $(files)

Client_Classes:
	$(call build,Client_Classes)

FindColor:
	$(call build,FindColor)
	
Input_Diag:
	$(call build,Input_Diag)

Window:
	$(call build,Window)

Window_Types:
	$(call build,Window_Types)

