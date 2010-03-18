#!/usr/bin/env python 
#
# QuickCheck convenience script
# Usage:
#     qCheck.py [FILE] [FILE] [FILE]
#
# This will look for all the functions starting 
# with prop_ and run them in ghci.


import sys
import subprocess


def find_Props(s):
    """Finds functions starting with prop_ 
    in the Haskell source file."""
    def get_Prop(s):
        res = ""
        for c in s:
            if c.isalnum() or c in ['_', "'"]:
                res += c
            else:
                break
        return res

    props = []
    for line in s.splitlines():
        if line.startswith("prop_"):
            props.append(get_Prop(line))
    return list(set(props))


def test_Props(srcFile, props):
    """Run the tests in ghci"""
    l = len(props)
    print "*" * 40
    print "Found %d props in %s." % (l, srcFile)
    if l == 0:
        print "Skipping."
    else:
        s = subprocess.Popen(["ghci"], stdin=subprocess.PIPE)
        s.stdin.write(":m +Test.QuickCheck\n")
        s.stdin.write(":l %s\n" % srcFile)
        s.stdin.write("\n".join(("putStrLn \"Running test: %s.\"\nTest.QuickCheck.quickCheck %s" % (p,p) for p in props)))
        s.stdin.write("\n:q\n")
        s.wait()


def main():
    for srcFile in sys.argv[1:]:
        f = open(srcFile, "r")
        props = find_Props(f.read())
        f.close()
        test_Props(srcFile, props)


if __name__ == "__main__":
    main()
