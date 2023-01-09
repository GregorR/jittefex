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
        "  --cxxflags\tC++ compiler flags." << std::endl <<
        "  --ldflags\tLinker flags." << std::endl <<
        "  --system-libs\tSystem libraries needed to link against Jittefex." << std::endl <<
        "  --libs\tLibraries needed to link against Jittefex." << std::endl;
}

int main(int argc, char **argv) {
    bool version = false,
         prefix = false,
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
    if (cxxflags)
        std::cout << "-I" << JITTEFEX_PREFIX << "/include" << std::endl;
    if (ldflags)
        std::cout << "-L" << JITTEFEX_PREFIX << "/lib" << std::endl;
    if (libs)
        std::cout << "-ljittefex" << std::endl;

    if (!cxxflags && !ldflags && !systemLibs && !libs)
        return 0;

    // Then pass them thru to llvm-config
#ifdef JITTEFEX_HAVE_LLVM
    std::vector<std::string> llvmConfigFlags;
    llvmConfigFlags.push_back("llvm-config");
    for (int ai = 1; ai < argc; ai++)
        llvmConfigFlags.push_back(argv[ai]);
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

#endif

    if (cxxflags)
        std::cout << "-std=c++17" << std::endl;

    return 0;
}
