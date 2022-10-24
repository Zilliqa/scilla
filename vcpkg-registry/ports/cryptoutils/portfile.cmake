vcpkg_from_github(
    OUT_SOURCE_PATH SOURCE_PATH
    REPO Zilliqa/cryptoutils
    REF a2e5e47ce6b21d9bc6fd05db18742aec0f0a81b1 #v8.2.0
    SHA512 86ae20427fdc982585dfe3da088d19232e9560fc8a03c123c64ebbfd6abad8c28133f05e950abdd504bdd3e9c7ba7c9aecea5f2295fe675bde191e497fbfb755
    HEAD_REF master
    PATCHES
      001-CMakeLists-txt.patch
)

vcpkg_from_github(
    OUT_SOURCE_PATH FFI_SOURCE_PATH
    REPO scipr-lab/libff
    REF f2067162520f91438b44e71a2cab2362f1c3cab4
    SHA512 0771c5b30c1cbf1794e8d8c2b06263f5b9f80f868d02440bca6bb7e2b6d2b4cf8b70c7ba9ae10dc8212fd28a3beb6f92d17f1b534f42862db86e3736ce387c75
    HEAD_REF master
)

vcpkg_from_github(
    OUT_SOURCE_PATH ATE_PAIRING_SOURCE_PATH
    REPO herumi/ate-pairing
    REF e69890125746cdaf25b5b51227d96678f76479fe
    SHA512 c5c41dc599b22b55a00f8a49e230bc88aec191f68434ba98d3504ac132067f5333cdae94c2e0ba6abb20aad2ce7c0be853ea146551f12a3ac05145fe9ae9d023
    HEAD_REF master
)

vcpkg_from_github(
    OUT_SOURCE_PATH XBYAK_SOURCE_PATH
    REPO herumi/xbyak
    REF f0a8f7faa27121f28186c2a7f4222a9fc66c283d
    SHA512 151cbc1ca4aa536703c5fbc7bc2e7a9c78d2d8799173fa3be9700864eb25d8b5f446c3e0f1879ac81783b255669834469b4d47938abdcc4d862f28c4766ff9e2
    HEAD_REF v5.63
)

file(RENAME ${FFI_SOURCE_PATH} ${SOURCE_PATH}/depends/libff)
file(RENAME ${ATE_PAIRING_SOURCE_PATH} ${SOURCE_PATH}/depends/libff/depends/ate-pairing)
file(RENAME ${XBYAK_SOURCE_PATH} ${SOURCE_PATH}/depends/libff/depends/xbyak)

vcpkg_cmake_configure(
    SOURCE_PATH ${SOURCE_PATH}
    OPTIONS
      -DCMAKE_CXX_FLAGS=-std=c++14
)

vcpkg_cmake_install()

file(REMOVE_RECURSE ${CURRENT_PACKAGES_DIR}/debug/include)

