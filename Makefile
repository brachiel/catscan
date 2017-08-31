
CATFLAP	= catFlap
BIN     = bin
STACK	= stack --local-bin-path ../$(BIN)

build:
	cd $(CATFLAP) && $(STACK) build && $(STACK) install

clean:
	rm -f $(BIN)/catFlap $(BIN)/catFlap.exe
	rm -f $(BIN)/catConfig $(BIN)/catConfig.exe
	cd $(CATFLAP) && $(STACK) clean

