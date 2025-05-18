use std::collections::HashMap;

use crate::asm_gen::{AsmFunctionDef, AsmInstruction, AsmOperand, AsmProgram, Reg};

pub fn resolve_pseudo_registers(asm_program: &AsmProgram) -> AsmProgram {
    let mut identifier_address_map = HashMap::<String, i32>::new();
    let mut offset: i32 = 0;

    fn resolve(
        operand: &AsmOperand,
        identifier_address_map: &mut HashMap<String, i32>,
        offset: &mut i32
    ) -> AsmOperand {
        return if let AsmOperand::Pseudo(identifier) = operand {
            if !identifier_address_map.contains_key(identifier) {
                identifier_address_map.insert(identifier.clone(), *offset);
                *offset = *offset - 4;
            }

            AsmOperand::Stack(*identifier_address_map.get(identifier).unwrap())
        } else {
            operand.clone()
        }
    }

    let mut transformed_instructions = asm_program.function_definition.instructions
        .iter()
        .map(|instruction| match instruction {
            AsmInstruction::Ret | AsmInstruction::AllocateStack(_) => instruction.clone(),
            AsmInstruction::Mov(src, dest) => AsmInstruction::Mov(
                    resolve(&src, &mut identifier_address_map, &mut offset),
                    resolve(&dest, &mut identifier_address_map, &mut offset),
            ),
            AsmInstruction::Unary(operator, operand) => AsmInstruction::Unary(
                operator.clone(),
                resolve(&operand, &mut identifier_address_map, &mut offset),
            ),
        })
        .flat_map(fixup_conflicting_operands)
        .collect::<Vec<AsmInstruction>>();

    transformed_instructions.insert(0, AsmInstruction::AllocateStack(-offset));

    return AsmProgram { function_definition: AsmFunctionDef { name: asm_program.function_definition.name.clone(), instructions: transformed_instructions } };
}

fn fixup_conflicting_operands(instruction: AsmInstruction) -> Vec<AsmInstruction> {
    return match instruction {
        AsmInstruction::Mov(AsmOperand::Stack(src_offset), AsmOperand::Stack(dest_offset)) => vec![
            AsmInstruction::Mov(AsmOperand::Stack(src_offset), AsmOperand::Register(Reg::R10)),
            AsmInstruction::Mov(AsmOperand::Register(Reg::R10), AsmOperand::Stack(dest_offset))
        ],
        _ => vec![instruction]
    }
}
