import lit.formats
from pathlib import Path
import re


class CustomTestFormat(lit.formats.ShTest):
    def execute(self, test, lit_config):
        filename = test.path_in_suite[-1]

        full_test_path = Path(test.suite.source_root, *test.path_in_suite)

        if filename.endswith('.custom.c') or filename.endswith('.ll'):
            cmds = []
        elif filename.endswith('.requires-input.c'):
            cmds = ['true']
        elif filename.endswith('.success.c'):
            cmds = [
                '%microcc %s >%t.ll',
                'opt -load-pass-plugin=libboundscheck.so -passes="microcc-bc" %t.ll -S -o %t.checked.ll',
                'llc --relocation-model=pic %t.checked.ll -o %t.checked.s',
                'clang++ %t.checked.s libruntime.a -o %t.checked.exe',
                '%t.checked.exe',
            ]
        elif filename.endswith('.static-fail.c'):
            cmds = [
                '%microcc %s >%t.ll',
                '%not --crash opt -load-pass-plugin=libboundscheck.so -passes="microcc-bc" %t.ll -S -o %t.checked.ll',
            ]
        elif filename.endswith('.dynamic-fail.c'):
            cmds = [
                '%microcc %s >%t.ll',
                'opt -load-pass-plugin=libboundscheck.so -passes="microcc-bc" %t.ll -S -o %t.checked.ll',
                'llc --relocation-model=pic %t.checked.ll -o %t.checked.s',
                'clang++ %t.checked.s libruntime.a -o %t.checked.exe',
                '%not --crash %t.checked.exe',
            ]
        else:
            assert False, f'Unhandled case for file {filename}'

        return lit.formats.ShTest(execute_external=True,
                                  preamble_commands=cmds).execute(test, lit_config)
