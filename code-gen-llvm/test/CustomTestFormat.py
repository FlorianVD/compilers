import lit.formats
from pathlib import Path
import re

class CustomTestFormat(lit.formats.ShTest):
    def execute(self, test, lit_config):
        filename = test.path_in_suite[-1]

        full_test_path = Path(test.suite.source_root, *test.path_in_suite)

        with full_test_path.open("r") as f:
            m = re.search(r"RUN-WITH-ARGS: (.*)", f.readline())

        extra_args = ""
        if m:
            extra_args = m.groups()[0]

        if filename.endswith('.custom.c'):
            cmds = []
        else:
            cmds = [
                f'[[ -f %s.stdin ]] && cp %s.stdin %t.stdin || touch %t.stdin',
                f'%microcc %s {extra_args} >%t.ll || {{ echo "" >%t.stdout && echo "Could not generate output: microcc failed. This generally means that your implementation of the code generator does handle certain cases incorrectly. Run the test suite locally for more information." >%t.stderr && false; }}',
                f'llc --relocation-model=pic %t.ll -o %t.s || {{ echo "" >%t.stdout && echo "Could not generate output: llc failed. This generally means that your code generator generates invalid LLVM IR. Run the test suite locally for more information." >%t.stderr && false; }}',
                f'clang++ %t.s libruntime.a -o %t.exe || {{ echo "" >%t.stdout && echo "Could not generate output: clang++ failed. This may mean that you\'re calling an unknown function." >%t.stderr && false; }}',
                f'bash %diff-command-output.sh %s %t -- %t.exe <%t.stdin || {{ echo "" >%t.stdout && echo "Some script failed. Warn the TAs ASAP." >%t.stderr && false; }}',
            ]

        return lit.formats.ShTest(execute_external=True,
                                  preamble_commands=cmds).execute(test, lit_config)
