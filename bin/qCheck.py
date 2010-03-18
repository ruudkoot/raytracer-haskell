#!/usr/bin/env python 
#
# QuickCheck convenience script
# Usage:
#     qCheck.py [FILE] [FILE] [FILE]
#
# This will look for all the functions starting 
# with prop_ and run them in ghci.
#


import sys
import subprocess


def find_Props(s):
    """Finds functions starting with prop_ 
    in the Haskell source file."""
    def get_Ident(s):
        res = ""
        for c in s:
            if c.isalnum() or c in ['_', "'"]:
                res += c
            else:
                break
        return res

    props = (get_Ident(line) for line in s.splitlines() 
             if line.startswith("prop_"))
    return list(set(props))


def test_Props(srcFile, props):
    """Run the tests in ghci"""

    test = "putStrLn \"Running test: %s.\"\nTest.QuickCheck.quickCheck %s"
    ghcInput = [":m +Test.QuickCheck", ":set +s", ":set +r"]
    l = len(props)
    if l == 0:
        print "Skipping %s. No props found." % srcFile
    else:
        ghcInput.append(":l %s" % srcFile)
        ghcInput.append("\n".join((test % (p,p) for p in props)))

    ghcInput.append(":q")
    s = subprocess.Popen(["ghci"], stdin=subprocess.PIPE)
    for x in ghcInput:
        s.stdin.write(x + "\n")
    s.wait()


def main():
    for srcFile in sys.argv[1:]:
        f = open(srcFile, "r")
        props = find_Props(f.read())
        f.close()
        test_Props(srcFile, props)


if __name__ == "__main__":
    main()
