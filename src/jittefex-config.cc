// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
/*
 * Copyright (c) 2022-2023 Gregor Richards
 *
 * Part of Jittefex, under the Apache License v2.0 with LLVM Exceptions.
 * See LICENSE for license information.
 */

#include "jittefex/config.h"

#include <iostream>
#include <string>
#include <vector>

#include <cstdio>
#include <spawn.h>
#include <sys/wait.h>

extern char **environ;

void usage() {
    std::cerr <<
        "Use: jittefex-config [options]" << std::endl <<
        "Options:" << std::endl <<
        "  --version\tPrint Jittefex version." << std::endl <<
        "  --prefix\tPrint the installation prefix." << std::endl <<
        "  --nogc\tUse a version of Jittefex with no GC support." << std::endl <<
        "  --notag\tUse an untagged (precise) GC." << std::endl <<
        "  --cflags\tC compiler flags." << std::endl <<
        "  --cxxflags\tC++ compiler flags." << std::endl <<
        "  --ldflags\tLinker flags." << std::endl <<
        "  --system-libs\tSystem libraries needed to link against Jittefex." << std::endl <<
        "  --libs\tLibraries needed to link against Jittefex." << std::endl;
}

int main(int argc, char **argv) {
    bool version = false,
         prefix = false,
         nogc = false,
         notag = false,
         cflags = false,
         cxxflags = false,
         ldflags = false,
         systemLibs = false,
         libs = false;

    for (int ai = 1; ai < argc; ai++) {
        std::string arg = argv[ai];
        if (arg == "--version")
            version = true;
        else if (arg == "--prefix")
            prefix = true;
        else if (arg == "--nogc")
            nogc = true;
        else if (arg == "--notag")
            notag = true;
        else if (arg == "--cflags")
            cflags = true;
        else if (arg == "--cxxflags")
            cxxflags = true;
        else if (arg == "--ldflags")
            ldflags = true;
        else if (arg == "--system-libs")
            systemLibs = true;
        else if (arg == "--libs")
            libs = true;
        else {
            usage();
            return 1;
        }
    }

    // First our own flags
    if (version)
        std::cout << JITTEFEX_VERSION << std::endl;
    if (prefix)
        std::cout << JITTEFEX_PREFIX << std::endl;
    if (cflags || cxxflags) {
        std::cout << "-I" << JITTEFEX_PREFIX << "/include/jittefex/config-";
        if (nogc)
            std::cout << "nogc";
        else if (notag)
            std::cout << "notag";
        else
            std::cout << "gc";
        std::cout << " -I" << JITTEFEX_PREFIX << "/include ";
    }
    if (ldflags)
        std::cout << "-L" << JITTEFEX_PREFIX << "/lib ";
    if (libs) {
        std::cout << "-ljittefex";
        if (nogc)
            std::cout << "-nogc";
        else if (notag)
            std::cout << "-notag";
        std::cout << " ";
    }
    std::cout << std::flush;

    if (!cflags && !cxxflags && !ldflags && !systemLibs && !libs) {
        std::cout << std::endl;
        return 0;
    }

    // Then pass them thru to llvm-config
#ifdef JITTEFEX_HAVE_LLVM
    std::vector<std::string> llvmConfigFlags;
    llvmConfigFlags.push_back("llvm-config");
    for (int ai = 1; ai < argc; ai++) {
        std::string arg = argv[ai];
        if (arg != "--nogc" && arg != "--notag")
            llvmConfigFlags.push_back(argv[ai]);
    }
    if (systemLibs || libs) {
        llvmConfigFlags.push_back("core");
        llvmConfigFlags.push_back("orcjit");
        llvmConfigFlags.push_back("native");
    }

    char **lcfc = new char *[llvmConfigFlags.size() + 1];
    for (int ai = 0; ai < llvmConfigFlags.size(); ai++)
        lcfc[ai] = (char *) llvmConfigFlags[ai].c_str();
    lcfc[llvmConfigFlags.size()] = nullptr;

    pid_t pid;
    if (posix_spawnp(
        &pid, JITTEFEX_LLVM_CONFIG, nullptr, nullptr, lcfc, environ
    )) {
        perror(JITTEFEX_LLVM_CONFIG);
        return 1;
    }
    waitpid(pid, nullptr, 0);

#else
    std::cout << std::endl;

#endif

    return 0;
}
