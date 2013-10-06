SOURCES=$(wildcard *.java)
CLASSES=$(SOURCES:.java=.class)

all: $(CLASSES)

$(CLASSES): %.class: %.java
	javac $<

clean:
	rm *.class
