use std::collections::HashMap;

use crate::asm_gen::{AsmBinaryOperator, AsmFunctionDef, AsmInstruction, AsmOperand, AsmProgram, Reg};

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
                *offset = *offset - 4;
                identifier_address_map.insert(identifier.clone(), *offset);
            }

            AsmOperand::Stack(*identifier_address_map.get(identifier).unwrap())
        } else {
            operand.clone()
        }
    }

    let mut transformed_instructions = asm_program.function_definition.instructions
        .iter()
        .map(|instruction| match instruction {
            AsmInstruction::Ret
            | AsmInstruction::AllocateStack(_)
            | AsmInstruction::Jmp(_)
            | AsmInstruction::JmpCC(_, _)
            | AsmInstruction::Label(_)
            | AsmInstruction::Cdq
            => instruction.clone(),
            AsmInstruction::Mov(src, dest) => AsmInstruction::Mov(
                resolve(&src, &mut identifier_address_map, &mut offset),
                resolve(&dest, &mut identifier_address_map, &mut offset),
            ),
            AsmInstruction::Unary(operator, operand) => AsmInstruction::Unary(
                operator.clone(),
                resolve(&operand, &mut identifier_address_map, &mut offset),
            ),
            AsmInstruction::Binary(operator, src_1, src_2) => AsmInstruction::Binary(
                operator.clone(),
                resolve(&src_1, &mut identifier_address_map, &mut offset),
                resolve(&src_2, &mut identifier_address_map, &mut offset),
            ),
            AsmInstruction::Idiv(src) => AsmInstruction::Idiv(
                resolve(&src, &mut identifier_address_map, &mut offset),
            ),
            AsmInstruction::Cmp(src_1, src_2) => AsmInstruction::Cmp(
                resolve(&src_1, &mut identifier_address_map, &mut offset),
                resolve(&src_2, &mut identifier_address_map, &mut offset),
            ),
            AsmInstruction::SetCC(cond_code, dest) => AsmInstruction::SetCC(
                cond_code.clone(),
                resolve(&dest, &mut identifier_address_map, &mut offset),
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
        AsmInstruction::Idiv(AsmOperand::Imm(int_value)) => vec![
            AsmInstruction::Mov(
                AsmOperand::Imm(int_value),
                AsmOperand::Register(Reg::R10),
            ),
            AsmInstruction::Idiv(
                AsmOperand::Register(Reg::R10),
            )
        ],
        AsmInstruction::Binary(AsmBinaryOperator::Add, AsmOperand::Stack(src_offset), AsmOperand::Stack(dest_offset)) => vec![
            AsmInstruction::Mov(AsmOperand::Stack(src_offset), AsmOperand::Register(Reg::R10)),
            AsmInstruction::Binary(AsmBinaryOperator::Add, AsmOperand::Register(Reg::R10), AsmOperand::Stack(dest_offset))
        ],
        AsmInstruction::Binary(AsmBinaryOperator::Sub, AsmOperand::Stack(src_offset), AsmOperand::Stack(dest_offset)) => vec![
            AsmInstruction::Mov(AsmOperand::Stack(src_offset), AsmOperand::Register(Reg::R10)),
            AsmInstruction::Binary(AsmBinaryOperator::Sub, AsmOperand::Register(Reg::R10), AsmOperand::Stack(dest_offset))
        ],
        AsmInstruction::Binary(AsmBinaryOperator::And, AsmOperand::Stack(src_offset), AsmOperand::Stack(dest_offset)) => vec![
            AsmInstruction::Mov(AsmOperand::Stack(src_offset), AsmOperand::Register(Reg::R10)),
            AsmInstruction::Binary(AsmBinaryOperator::And, AsmOperand::Register(Reg::R10), AsmOperand::Stack(dest_offset))
        ],
        AsmInstruction::Binary(AsmBinaryOperator::Xor, AsmOperand::Stack(src_offset), AsmOperand::Stack(dest_offset)) => vec![
            AsmInstruction::Mov(AsmOperand::Stack(src_offset), AsmOperand::Register(Reg::R10)),
            AsmInstruction::Binary(AsmBinaryOperator::Xor, AsmOperand::Register(Reg::R10), AsmOperand::Stack(dest_offset))
        ],
        AsmInstruction::Binary(AsmBinaryOperator::Or, AsmOperand::Stack(src_offset), AsmOperand::Stack(dest_offset)) => vec![
            AsmInstruction::Mov(AsmOperand::Stack(src_offset), AsmOperand::Register(Reg::R10)),
            AsmInstruction::Binary(AsmBinaryOperator::Or, AsmOperand::Register(Reg::R10), AsmOperand::Stack(dest_offset))
        ],
        AsmInstruction::Binary(AsmBinaryOperator::Shl, AsmOperand::Stack(src_offset), AsmOperand::Stack(dest_offset)) => vec![
            AsmInstruction::Mov(AsmOperand::Stack(src_offset), AsmOperand::Register(Reg::CX)),
            AsmInstruction::Mov(AsmOperand::Stack(dest_offset), AsmOperand::Register(Reg::BX)),
            AsmInstruction::Binary(AsmBinaryOperator::Shl, AsmOperand::Register(Reg::CL), AsmOperand::Register(Reg::BX)),
            AsmInstruction::Mov(AsmOperand::Register(Reg::BX), AsmOperand::Stack(dest_offset)),
        ],
        AsmInstruction::Binary(AsmBinaryOperator::Shr, AsmOperand::Stack(src_offset), AsmOperand::Stack(dest_offset)) => vec![
            AsmInstruction::Mov(AsmOperand::Stack(src_offset), AsmOperand::Register(Reg::CX)),
            AsmInstruction::Mov(AsmOperand::Stack(dest_offset), AsmOperand::Register(Reg::BX)),
            AsmInstruction::Binary(AsmBinaryOperator::Shr, AsmOperand::Register(Reg::CL), AsmOperand::Register(Reg::BX)),
            AsmInstruction::Mov(AsmOperand::Register(Reg::BX), AsmOperand::Stack(dest_offset)),
        ],
        AsmInstruction::Binary(AsmBinaryOperator::Mult, src, AsmOperand::Stack(dest_offset)) => vec![
            AsmInstruction::Mov(AsmOperand::Stack(dest_offset), AsmOperand::Register(Reg::R11)),
            AsmInstruction::Binary(AsmBinaryOperator::Mult, src, AsmOperand::Register(Reg::R11)),
            AsmInstruction::Mov(AsmOperand::Register(Reg::R11), AsmOperand::Stack(dest_offset)),
        ],
        AsmInstruction::Cmp(AsmOperand::Stack(val_1_offset), AsmOperand::Stack(val_2_offset)) => vec![
            AsmInstruction::Mov(AsmOperand::Stack(val_1_offset), AsmOperand::Register(Reg::R10)),
            AsmInstruction::Cmp(AsmOperand::Register(Reg::R10), AsmOperand::Stack(val_2_offset)),
        ],
        AsmInstruction::Cmp(val_1, AsmOperand::Imm(int_value)) => vec![
            AsmInstruction::Mov(AsmOperand::Imm(int_value), AsmOperand::Register(Reg::R11)),
            AsmInstruction::Cmp(val_1, AsmOperand::Register(Reg::R11)),
        ],
        _ => vec![instruction]
    }
}
