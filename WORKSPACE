load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive", "http_jar")

http_jar(
    name = "antlr4",
    sha256 = "f41dce7441d523baf9769cb7756a00f27a4b67e55aacab44525541f62d7f6688",
    url = "https://www.antlr.org/download/antlr-4.7.1-complete.jar",
)

http_archive(
    name = "antlr4_cpp_runtime",
    build_file = "@//:BUILD.antlr4_cpp_runtime",
    sha256 = "4d0714f441333a63e50031c9e8e4890c78f3d21e053d46416949803e122a6574",
    strip_prefix = "antlr4-4.7.1/runtime/Cpp",
    url = "https://github.com/antlr/antlr4/archive/4.7.1.tar.gz",
)

http_archive(
    name = "re2",
    sha256 = "15818006416421edb583f8f1502e77eca3338bb3c19925226a7e5b65e746f279",
    strip_prefix = "re2-25bfefa0eefb53aa3ea28d26194f10362ec4cab3",
    url = "https://github.com/google/re2/archive/25bfefa0eefb53aa3ea28d26194f10362ec4cab3.tar.gz",
)

http_archive(
    name = "com_google_absl",
    sha256 = "80f5da966b977cd36127c9614074b16e2d3a74e9066d6ee91c7916d798faa333",
    strip_prefix = "abseil-cpp-e5be80532b5d998813f9db952d2cc5401b1532df",
    url = "https://github.com/abseil/abseil-cpp/archive/e5be80532b5d998813f9db952d2cc5401b1532df.tar.gz",
)

http_archive(
    name = "cpp_btree",
    build_file = "@//:BUILD.cpp_btree",
    sha256 = "e86d047ef509d70f706d526301dc64b6935826d38649f834de49cd11a3018d01",
    strip_prefix = "cpp-btree-92ec61e4b8bf182c5c49ebf6540dac62d569d090",
    url = "https://github.com/algorithm-ninja/cpp-btree/archive/92ec61e4b8bf182c5c49ebf6540dac62d569d090.tar.gz",
)
