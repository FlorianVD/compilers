#include "codegen-x64/optimise-x64.hpp"

#include "codegen-x64/module.hpp"

#include "llvm/ADT/Statistic.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>
#include <iterator>
#include <unordered_set>

#define DEBUG_TYPE "optimiser-x64"

// Command-line options to select which optimisation to apply.
llvm::cl::bits<codegen_x64::Optimisations> OptimisationsBits(
    llvm::cl::desc("Available peephole optimisations:"),
    llvm::cl::values(clEnumValN(codegen_x64::Optimisations::NopElimination,
                                "optimisation-nop-elimination",
                                "Eliminate NOP instructions"),
                     clEnumValN(codegen_x64::Optimisations::Assignment1,
                                "optimisation-1", "Your optimisation 1"),
                     clEnumValN(codegen_x64::Optimisations::Assignment2,
                                "optimisation-2", "Your optimisation 2"),
                     clEnumValN(codegen_x64::Optimisations::Assignment3,
                                "optimisation-3", "Your optimisation 3")));

// Statistic to see how many NOPs were removed.
STATISTIC(NumNopsEliminated,
          "The number of NOP instructions that were eliminated");

void codegen_x64::OptimiserX64::optimise(Module &module) {
    bool changed = true;

    while (changed) {
        changed = false;

        if (OptimisationsBits.isSet(Optimisations::NopElimination))
            changed |= optimiseNopElimination(module);

        if (OptimisationsBits.isSet(Optimisations::Assignment1))
            changed |= optimise1(module);

        if (OptimisationsBits.isSet(Optimisations::Assignment2))
            changed |= optimise2(module);

        if (OptimisationsBits.isSet(Optimisations::Assignment3))
            changed |= optimise3(module);
    }
}

bool codegen_x64::OptimiserX64::optimiseNopElimination(Module &mod) {
    bool changed = false;

    // Remove all "nop" instructions from all basic blocks.
    for (auto &bbl : mod.blocks) {
        auto &insns = bbl.instructions;
        auto it = std::remove_if(
            std::begin(insns), std::end(insns),
            [](const Instruction &ins) { return ins.opcode == "nop"; });

        // Update statistic
        NumNopsEliminated += std::distance(it, std::end(insns));

        // Check if anything was removed
        if (it != std::end(insns))
            changed = true;

        insns.erase(it, std::end(insns));
    }

    return changed;
}

namespace codegen_x64 {

bool is_push(const Instruction &instruction) {
    return instruction.opcode == "pushq";
}

bool is_pop(const Instruction &instruction) {
    return instruction.opcode == "popq";
}

bool have_same_operands(const Instruction &i1, const Instruction &i2) {
    if (i1.operands.size() != i2.operands.size())
        return false;

    for (size_t i = 0; i < i1.operands.size(); i++) {
        if (i1.operands[i] != i2.operands[i])
            return false;
    }
    return true;
}

bool single_operand_is_constant(const Instruction &instruction) {
    if (instruction.operands.size() != 1)
        return false;
    return instruction.operands[0][0] == '$';
}

/*
// Finds the next instruction containing the register
// This is unused, as it is too simple to use effectively, (e.g. it does not
detect combined %eax and %rax usage) int
find_next_instruction_with_register(std::vector<Instruction> &instructions, int
start_offset, std::string reg) {

    for (size_t i = start_offset; i < instructions.size(); i++) {
        for (auto operand : instructions[i].operands) {
            if (operand == reg && ignore_regs.count(reg) == 0)
                return i;
        }
    }
    return -1;
}
*/

} // namespace codegen_x64

bool codegen_x64::OptimiserX64::optimise1(Module &mod) {
    // ASSIGNMENT: Implement your first peephole optimisation here.
    // Return true only if you have changed the assembly

    // Optimization 1: remove redunant push-pops and pop-pushes
    size_t pushpops_removed = 0;
    for (auto &block : mod.blocks) {
        const auto instructions = block.instructions;
        size_t index = 0;
        while (index < block.instructions.size() - 1) {
            Instruction i1 = block.instructions[index];
            Instruction i2 = block.instructions[index + 1];
            if (is_push(i1) && is_pop(i2) && have_same_operands(i1, i2)) {
                block.instructions[index] = Instruction{"nop", {}, "Optimisation 1: remove redundant push"};
                block.instructions[index + 1] = Instruction{"nop", {}, "Optimisation 1: remove redundant pop"};
                pushpops_removed += 2;
            }
            if (is_pop(i1) && is_push(i2) && have_same_operands(i1, i2)) {
                block.instructions[index] = Instruction{"nop", {}, "Optimisation 1: remove redundant pop"};
                block.instructions[index + 1] = Instruction{"nop", {}, "Optimisation 1: remove redundant push"};
                pushpops_removed += 2;
            }
            index++;
        }
    }
    return pushpops_removed > 0;
}

bool codegen_x64::OptimiserX64::optimise2(Module &mod) {
    // ASSIGNMENT: Implement your second peephole optimisation here.
    // Return true only if you have changed the assembly.

    size_t movs_added = 0;
    for (auto &block : mod.blocks) {
        const auto instructions = block.instructions;
        size_t index = 0;
        while (index < block.instructions.size() - 1) {
            Instruction i1 = block.instructions[index];
            Instruction i2 = block.instructions[index + 1];
            if (is_push(i1) && is_pop(i2) && single_operand_is_constant(i1)) {
                block.instructions[index] =
                    Instruction{"movq",
                                {i1.operands[0], i2.operands[0]},
                                "Optimisation 2: Move constant value"};
                block.instructions[index + 1] = Instruction{"nop", {}, "Optimisation 2: replace pop with nop"};
                movs_added++;
            }
            index++;
        }
    }

    return movs_added > 0;
}

bool codegen_x64::OptimiserX64::optimise3(Module &mod) {
    // ASSIGNMENT: Implement your third peephole optimisation here.
    // Return true only if you have changed the assembly.
    return false;
}

void codegen_x64::OptimiserX64::makeNop(Instruction &ins) {
    ins.opcode = "nop";
    ins.operands.clear();
}
