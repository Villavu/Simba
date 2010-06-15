
.PHONY: default clean

files := Client_Classes FindColor Input_Diag Window DTM TMWindow client_classes_dependencies
build = dot $(1).dot -Tpng > $(1).png

default: dotit
	
clean:
	rm -f *.png

dotit: $(files)

Client_Classes:
	$(call build,Client_Classes)

client_classes_dependencies:
	$(call build,client_classes_dependencies)

FindColor:
	$(call build,FindColor)
	
Input_Diag:
	$(call build,Input_Diag)

Window:
	$(call build,Window)

TMWindow:
	$(call build,TMWindow)

DTM:
	$(call build,DTM)
