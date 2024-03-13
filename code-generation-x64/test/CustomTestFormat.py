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
                f'%microcc %s {extra_args} >%t.s || {{ echo "" >%t.stdout && echo "Could not generate output: microcc failed. This generally means that your implementation of the code generator does handle certain cases correctly. Run the test suite locally for more information." >%t.stderr && false; }}',
                f'g++ %t.s libruntime.a -o %t.exe || {{ echo "" >%t.stdout && echo "Could not generate output: g++ failed. This generally means that your code generator generates invalid assembly. Run the test suite locally for more information." >%t.stderr && false; }}',
                f'%diff-command-output.sh %s %t -- %t.exe <%t.stdin'
            ]

        return lit.formats.ShTest(execute_external=True,
                                  preamble_commands=cmds).execute(test, lit_config)
