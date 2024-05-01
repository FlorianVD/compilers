import lit.formats
from pathlib import Path
import re


class CustomTestFormat(lit.formats.ShTest):
    def execute(self, test, lit_config):
        filename = test.path_in_suite[-1]

        if filename.endswith('.custom.c') or filename.endswith('.custom.ll'):
            cmds = []
        elif test.path_in_suite[0] == 'examples':
            cmds = ['true']
        else:
            assert False, f'Unhandled case for file {filename}'

        return lit.formats.ShTest(execute_external=True,
                                  preamble_commands=cmds).execute(test, lit_config)
