
all:
	-git clone https://github.com/vscosta/yap.git
	-mkdir -p yap/build
	-cd yap/build ; cmake .. -DBUILD_SHARED_LIBS=OFF -DBUILD_STATIC_LIBS=OFF -DWITH_PACKAGES=OFF -DWITH_R=OFF -DWITH_Raptor2=OFF
	-$(MAKE) -C yap/build
