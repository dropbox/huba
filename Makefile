
all: thrift
	cabal build

dist/build/Main/Main: thrift
	cabal build Main
main: dist/build/Main/Main

dist/build/IngestorTestClient/IngestorTestClient: thrift
	cabal build IngestorTestClient
test: dist/build/IngestorTestClient/IngestorTestClient

src/Shared/Thrift/Generated/Huba_Types.hs: src/Shared/Thrift/Huba.thrift
	mkdir -p src/Shared/Thrift/Generated
	thrift --out src/Shared/Thrift/Generated --gen hs src/Shared/Thrift/Huba.thrift
thrift: src/Shared/Thrift/Generated/Huba_Types.hs

clean:
	rm -r src/Shared/Thrift/Generated
	rm -r dist

.PHONY : clean
