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
                f'%diff-command-output.sh %s %t -- %microcc %s {extra_args}'
            ]

        return lit.formats.ShTest(execute_external=True,
                                  preamble_commands=cmds).execute(test, lit_config)
