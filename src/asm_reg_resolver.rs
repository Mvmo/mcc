use std::collections::HashMap;

use crate::asm_gen::{AsmBinaryOperator, AsmFunctionDef, AsmInstruction, AsmOperand, AsmProgram, Reg};

pub fn resolve_pseudo_registers(asm_program: &AsmProgram) -> AsmProgram {
    use AsmInstruction::*;
    use AsmOperand::*;

    let mut identifier_address_map = HashMap::<String, i32>::new();
    let mut offset: i32 = 0;

    fn resolve(
        operand: &AsmOperand,
        identifier_address_map: &mut HashMap<String, i32>,
        offset: &mut i32
    ) -> AsmOperand {
        return if let Pseudo(identifier) = operand {
            if !identifier_address_map.contains_key(identifier) {
                *offset = *offset - 4;
                identifier_address_map.insert(identifier.clone(), *offset);
            }

            Stack(*identifier_address_map.get(identifier).unwrap())
        } else {
            operand.clone()
        }
    }

    let mut transformed_instructions = asm_program.function_definition.instructions
        .iter()
        .map(|instruction| match instruction {
            Ret
            | AllocateStack(_)
            | Jmp(_)
            | JmpCC(_, _)
            | Label(_)
            | Cdq
            => instruction.clone(),
            Mov(src, dest) => Mov(
                resolve(&src, &mut identifier_address_map, &mut offset),
                resolve(&dest, &mut identifier_address_map, &mut offset),
            ),
            Unary(operator, operand) => Unary(
                operator.clone(),
                resolve(&operand, &mut identifier_address_map, &mut offset),
            ),
            Binary(operator, src_1, src_2) => Binary(
                operator.clone(),
                resolve(&src_1, &mut identifier_address_map, &mut offset),
                resolve(&src_2, &mut identifier_address_map, &mut offset),
            ),
            Idiv(src) => Idiv(
                resolve(&src, &mut identifier_address_map, &mut offset),
            ),
            Cmp(src_1, src_2) => Cmp(
                resolve(&src_1, &mut identifier_address_map, &mut offset),
                resolve(&src_2, &mut identifier_address_map, &mut offset),
            ),
            SetCC(cond_code, dest) => SetCC(
                cond_code.clone(),
                resolve(&dest, &mut identifier_address_map, &mut offset),
            ),
        })
        .flat_map(fixup_conflicting_operands)
        .collect::<Vec<AsmInstruction>>();

    transformed_instructions.insert(0, AllocateStack(-offset));

    return AsmProgram { function_definition: AsmFunctionDef { name: asm_program.function_definition.name.clone(), instructions: transformed_instructions } };
}

fn fixup_conflicting_operands(instruction: AsmInstruction) -> Vec<AsmInstruction> {
    use AsmInstruction::*;
    use AsmOperand::*;
    use AsmBinaryOperator::*;

    return match instruction {
        Mov(Stack(src_offset), Stack(dest_offset)) => vec![
            Mov(Stack(src_offset), Register(Reg::R10)),
            Mov(Register(Reg::R10), Stack(dest_offset))
        ],
        Idiv(Imm(int_value)) => vec![
            Mov(
                Imm(int_value),
                Register(Reg::R10),
            ),
            Idiv(
                Register(Reg::R10),
            )
        ],
        Binary(Add, Stack(src_offset), Stack(dest_offset)) => vec![
            Mov(Stack(src_offset), Register(Reg::R10)),
            Binary(Add, Register(Reg::R10), Stack(dest_offset))
        ],
        Binary(Sub, Stack(src_offset), Stack(dest_offset)) => vec![
            Mov(Stack(src_offset), Register(Reg::R10)),
            Binary(Sub, Register(Reg::R10), Stack(dest_offset))
        ],
        Binary(And, Stack(src_offset), Stack(dest_offset)) => vec![
            Mov(Stack(src_offset), Register(Reg::R10)),
            Binary(And, Register(Reg::R10), Stack(dest_offset))
        ],
        Binary(Xor, Stack(src_offset), Stack(dest_offset)) => vec![
            Mov(Stack(src_offset), Register(Reg::R10)),
            Binary(Xor, Register(Reg::R10), Stack(dest_offset))
        ],
        Binary(Or, Stack(src_offset), Stack(dest_offset)) => vec![
            Mov(Stack(src_offset), Register(Reg::R10)),
            Binary(Or, Register(Reg::R10), Stack(dest_offset))
        ],
        Binary(Shl, Stack(src_offset), Stack(dest_offset)) => vec![
            Mov(Stack(src_offset), Register(Reg::CX)),
            Mov(Stack(dest_offset), Register(Reg::BX)),
            Binary(Shl, Register(Reg::CL), Register(Reg::BX)),
            Mov(Register(Reg::BX), Stack(dest_offset)),
        ],
        Binary(Shr, Stack(src_offset), Stack(dest_offset)) => vec![
            Mov(Stack(src_offset), Register(Reg::CX)),
            Mov(Stack(dest_offset), Register(Reg::BX)),
            Binary(Shr, Register(Reg::CL), Register(Reg::BX)),
            Mov(Register(Reg::BX), Stack(dest_offset)),
        ],
        Binary(Mult, src, Stack(dest_offset)) => vec![
            Mov(Stack(dest_offset), Register(Reg::R11)),
            Binary(Mult, src, Register(Reg::R11)),
            Mov(Register(Reg::R11), Stack(dest_offset)),
        ],
        Cmp(Stack(val_1_offset), Stack(val_2_offset)) => vec![
            Mov(Stack(val_1_offset), Register(Reg::R10)),
            Cmp(Register(Reg::R10), Stack(val_2_offset)),
        ],
        Cmp(val_1, Imm(int_value)) => vec![
            Mov(Imm(int_value), Register(Reg::R11)),
            Cmp(val_1, Register(Reg::R11)),
        ],
        _ => vec![instruction]
    }
}
