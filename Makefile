
all: thrift
	cabal build

dist/build/Main/Main: thrift
	cabal build Main
main: dist/build/Main/Main

dist/build/IngestorTestClient/IngestorTestClient: thrift
	cabal build IngestorTestClient
test: dist/build/IngestorTestClient/IngestorTestClient

Shared/Thrift/Generated/Huba_Types.hs: Shared/Thrift/Huba.thrift
	mkdir -p Shared/Thrift/Generated
	thrift --out Shared/Thrift/Generated --gen hs Shared/Thrift/Huba.thrift
thrift: Shared/Thrift/Generated/Huba_Types.hs

clean:
	rm -r Shared/Thrift/Generated
	rm -r dist

.PHONY : clean
