genrule(
    name = "generated",
    srcs = ["checktestdata.g4"],
    outs = [
        "checktestdataBaseListener.cpp",
        "checktestdataBaseListener.h",
        "checktestdataLexer.cpp",
        "checktestdataLexer.h",
        "checktestdataListener.cpp",
        "checktestdataListener.h",
        "checktestdataParser.cpp",
        "checktestdataParser.h",
    ],
    cmd = "$(location :antlr4) -Dlanguage=Cpp -o $(@D) checktestdata.g4",
    tools = [":antlr4"],
)

java_binary(
    name = "antlr4",
    main_class = "org.antlr.v4.Tool",
    runtime_deps = [
        "@antlr4//jar",
    ],
)

cc_library(
    name = "parser",
    srcs = [":generated"],
    deps = [
        "@antlr4_cpp_runtime//:runtime",
    ],
)

cc_binary(
    name = "checktestdata",
    srcs = ["checktestdata.cc"],
    deps = [
        ":command",
        ":expression",
        ":parser",
        ":stream",
        ":value",
        ":variable",
        "@com_google_absl//absl/strings",
        "@re2",
    ],
)

cc_library(
    name = "stream",
    hdrs = ["stream.h"],
)

cc_library(
    name = "expression",
    srcs = ["expression.cc"],
    hdrs = ["expression.h"],
    deps = [
        ":parser",
        ":value",
        ":variable",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "value",
    srcs = ["value.cc"],
    hdrs = ["value.h"],
    linkopts = [
        "-lgmp",
    ],
    deps = [
        ":parser",
        "@com_google_absl//absl/strings",
    ],
)

cc_library(
    name = "variable",
    srcs = ["variable.cc"],
    hdrs = ["variable.h"],
    deps = [
        ":parser",
        ":value",
        "@cpp_btree//:btree_map",
    ],
)

cc_library(
    name = "command",
    srcs = [],
    hdrs = ["command.h"],
)
