#!/bin/bash

find $1 | grep -E -v "Contrib|HTML|lib|dpkg|\.gradle|\.class|\.svn|\.hg|\.git|\.html|\.jar|\.pom|\.pom.sha1|Debug|Release|Master|bin"  > $1/index