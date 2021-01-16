#include "vcs.h"

unsigned short addr_absolute() {
	/*
	Absolute

	Instructions using absolute addressing contain a full 16 bit address to identify the target location.
	*/
	return operand_2bytes() & 0x1fff;
}

unsigned short addr_absolute_x() {
	/*
	Absolute,X

	The address to be accessed by an instruction using X register indexed absolute addressing is computed by taking the 16 bit address from the instruction and added the contents of the X register.
	For example if X contains $92 then an STA $2000,X instruction will store the accumulator at $2092 (e.g. $2000 + $92).
	*/
	return (operand_2bytes() + index_register_x) & 0x1fff;
}

unsigned short addr_absolute_y() {
	/*
	Absolute,Y

	The Y register indexed absolute addressing mode is the same as the previous mode only with the contents of the Y register added to the 16 bit address from the instruction.
	*/
	return (operand_2bytes() + index_register_y) & 0x1fff;
}

unsigned short addr_indirect() {
	/*
	Indirect

	JMP is the only 6502 instruction to support indirection.
	The instruction contains a 16 bit address which identifies the location of the least significant byte of another 16 bit memory address which is the real target of the instruction.
	For example if location $0120 contains $FC and location $0121 contains $BA then the instruction JMP ($0120) will cause the next instruction execution to occur at $BAFC (e.g. the contents of $0120 and $0121).
	*/
	unsigned char byte1;
	unsigned char byte2;

	byte1 = memory[operand_2bytes()];
	byte2 = memory[operand_2bytes() + 1];
	return (byte1 | (byte2 << BYTE_SIZE)) & 0x1fff;
}

unsigned short addr_indirect_x() {
	/*
	Indexed Indirect

	Indexed indirect addressing is normally used in conjunction with a table of address held on zero page.
	The address of the table is taken from the instruction and the X register added to it (with zero page wrap around) to give the location of the least significant byte of the target address.
	*/
	unsigned char address;
	unsigned char byte1;
	unsigned char byte2;

	fetch();
	address = instruction + index_register_x;
	byte1 = memory[address];
	byte2 = memory[address + 1];
	return (byte1 | (byte2 << BYTE_SIZE)) & 0x1fff;
}

unsigned short addr_indirect_y() {
	/*
	Indirect Indexed

	Indirect indirect addressing is the most common indirection mode used on the 6502.
	In instruction contains the zero page location of the least significant byte of 16 bit address.
	The Y register is dynamically added to this value to generated the actual target address for operation.
	*/
	unsigned char byte1;
	unsigned char byte2;

	fetch();
	byte1 = memory[instruction];
	byte2 = memory[instruction + 1];
	return ((byte1 | (byte2 << BYTE_SIZE)) + index_register_y) & 0x1fff;
}

signed char addr_relative() {
	/*
	Relative

	Relative addressing mode is used by branch instructions (e.g. BEQ, BNE, etc.) which contain a signed 8 bit relative offset (e.g. -128 to +127) which is added to program counter if the condition is true.
	As the program counter itself is incremented during instruction execution by two the effective address range for the target instruction must be with -126 to +129 bytes of the branch.
	*/
	fetch();
	return instruction;
}

unsigned short addr_zero_page() {
	/*
	Zero Page

	An instruction using zero page addressing mode has only an 8 bit address operand.
	This limits it to addressing only the first 256 bytes of memory (e.g. $0000 to $00FF) where the most significant byte of the address is always zero.
	*/
	fetch();
	return (0x0000 | instruction) & 0x1fff;
}

unsigned short addr_zero_page_x() {
	/*
	Zero Page,X

	The address to be accessed by an instruction using indexed zero page addressing is calculated by taking the 8 bit zero page address from the instruction and adding the current value of the X register to it.
	For example if the X register contains $0F and the instruction LDA $80,X is executed then the accumulator will be loaded from $008F (e.g. $80 + $0F => $8F).
	The address calculation wraps around if the sum of the base address and the register exceed $FF.
	If we repeat the last example but with $FF in the X register then the accumulator will be loaded from $007F (e.g. $80 + $FF => $7F) and not $017F.
	*/
	fetch();
	return (instruction + index_register_x) & 0x1fff;
}

unsigned short addr_zero_page_y() {
	/*
	Zero Page,Y

	The address to be accessed by an instruction using indexed zero page addressing is calculated by taking the 8 bit zero page address from the instruction and adding the current value of the Y register to it.
	This mode can only be used with the LDX and STX instructions.
	*/
	fetch();
	return (instruction + index_register_y) & 0x1fff;
}

void check_negative(unsigned char data) {
	if (data & (1 << STATUS_BIT_NEGATIVE) > 0) {
		flag_negative_set();
	} else {
		flag_negative_unset();
	}
}

void check_overflow(unsigned short data) {
	if (data > 0xff) {
		flag_overflow_set();
	} else {
		flag_overflow_unset();
	}
}

void check_zero(unsigned char data) {
	if (data == 0) {
		flag_zero_set();
	} else {
		flag_zero_unset();
	}
}

void cycle() {
	while(running) {
		fetch();
		decode();
		update();
	}
}

unsigned char data_absolute() {
	return memory[addr_absolute()];
}

unsigned char data_absolute_x() {
	return memory[addr_absolute_x()];
}

unsigned char data_absolute_y() {
	return memory[addr_absolute_y()];
}

unsigned char data_immediate() {
	/*
	Immediate

	Immediate addressing allows the programmer to directly specify an 8 bit constant within the instruction.
	*/
	fetch();
	return instruction;
}

unsigned char data_indirect_x() {
	return memory[addr_indirect_x()];
}

unsigned char data_indirect_y() {
	return memory[addr_indirect_y()];
}

unsigned char data_zero_page() {
	return memory[addr_zero_page()];
}

unsigned char data_zero_page_x() {
	return memory[addr_zero_page_x()];
}

unsigned char data_zero_page_y() {
	return memory[addr_zero_page_y()];
}

void decode() {
	switch (instruction) {
		case 0x00:
			instruction_brk();
		break;

		case 0x01:
			instruction_ora(data_indirect_x());
		break;

		case 0x05:
			instruction_ora(data_zero_page());
		break;

		case 0x06:
			instruction_asl(addr_zero_page());
		break;

		case 0x08:
			instruction_php();
		break;

		case 0x09:
			instruction_ora(data_immediate());
		break;

		case 0x0a:
			instruction_asl(0);
		break;

		case 0x0d:
			instruction_ora(data_absolute());
		break;

		case 0x0e:
			instruction_asl(addr_absolute());
		break;

		case 0x10:
			instruction_bpl(addr_relative());
		break;

		case 0x11:
			instruction_ora(data_indirect_y());
		break;

		case 0x15:
			instruction_ora(data_zero_page_x());
		break;

		case 0x16:
			instruction_asl(addr_zero_page_x());
		break;

		case 0x18:
			instruction_clc();
		break;

		case 0x19:
			instruction_ora(data_absolute_y());
		break;

		case 0x1d:
			instruction_ora(data_absolute_x());
		break;

		case 0x1e:
			instruction_asl(addr_absolute_x());
		break;

		case 0x20:
			instruction_jsr(addr_absolute());
		break;

		case 0x21:
			instruction_and(data_indirect_x());
		break;

		case 0x24:
			instruction_bit(data_zero_page());
		break;

		case 0x25:
			instruction_and(data_zero_page());
		break;

		case 0x26:
			instruction_rol(addr_zero_page());
		break;

		case 0x28:
			instruction_plp();
		break;

		case 0x29:
			instruction_and(data_immediate());
		break;

		case 0x2a:
			instruction_rol(0);
		break;

		case 0x2c:
			instruction_bit(data_absolute());
		break;

		case 0x2d:
			instruction_and(data_absolute());
		break;

		case 0x2e:
			instruction_rol(addr_absolute());
		break;

		case 0x30:
			instruction_bmi(addr_relative());
		break;

		case 0x31:
			instruction_and(data_indirect_y());
		break;

		case 0x35:
			instruction_and(data_zero_page_x());
		break;

		case 0x36:
			instruction_rol(addr_zero_page_x());
		break;

		case 0x38:
			instruction_sec();
		break;

		case 0x39:
			instruction_and(data_absolute_y());
		break;

		case 0x3d:
			instruction_and(data_absolute_x());
		break;

		case 0x3e:
			instruction_rol(addr_absolute_x());
		break;

		case 0x40:
			instruction_rti();
		break;

		case 0x41:
			instruction_eor(data_indirect_x());
		break;

		case 0x45:
			instruction_eor(data_zero_page());
		break;

		case 0x46:
			instruction_lsr(addr_zero_page());
		break;

		case 0x48:
			instruction_pha();
		break;

		case 0x49:
			instruction_eor(data_immediate());
		break;

		case 0x4a:
			instruction_lsr(0);
		break;

		case 0x4c:
			instruction_jmp(addr_absolute());
		break;

		case 0x4d:
			instruction_eor(data_absolute());
		break;

		case 0x4e:
			instruction_lsr(addr_absolute());
		break;

		case 0x50:
			instruction_bvc(addr_relative());
		break;

		case 0x51:
			instruction_eor(data_indirect_y());
		break;

		case 0x55:
			instruction_eor(data_zero_page_x());
		break;

		case 0x56:
			instruction_lsr(addr_zero_page_x());
		break;

		case 0x58:
			instruction_cli();
		break;

		case 0x59:
			instruction_eor(data_absolute_y());
		break;

		case 0x5d:
			instruction_eor(data_absolute_x());
		break;

		case 0x5e:
			instruction_lsr(addr_absolute_x());
		break;

		case 0x60:
			instruction_rts();
		break;

		case 0x61:
			instruction_adc(data_indirect_x());
		break;

		case 0x65:
			instruction_adc(data_zero_page());
		break;

		case 0x66:
			instruction_ror(addr_zero_page());
		break;

		case 0x68:
			instruction_pla();
		break;

		case 0x69:
			instruction_adc(data_immediate());
		break;

		case 0x6a:
			instruction_ror(0);
		break;

		case 0x6c:
			instruction_jmp(addr_indirect());
		break;

		case 0x6d:
			instruction_adc(data_absolute());
		break;

		case 0x6e:
			instruction_ror(addr_absolute());
		break;

		case 0x70:
			instruction_bvs(addr_relative());
		break;

		case 0x71:
			instruction_adc(data_indirect_y());
		break;

		case 0x75:
			instruction_adc(data_zero_page_x());
		break;

		case 0x76:
			instruction_ror(addr_zero_page_x());
		break;

		case 0x78:
			instruction_sei();
		break;

		case 0x79:
			instruction_adc(data_absolute_y());
		break;

		case 0x7d:
			instruction_adc(data_absolute_x());
		break;

		case 0x7e:
			instruction_ror(addr_absolute_x());
		break;

		case 0x81:
			instruction_sta(addr_indirect_x());
		break;

		case 0x84:
			instruction_sty(addr_zero_page());
		break;

		case 0x85:
			instruction_sta(addr_zero_page());
		break;

		case 0x86:
			instruction_stx(addr_zero_page());
		break;

		case 0x88:
			instruction_dey();
		break;

		case 0x8a:
			instruction_txa();
		break;

		case 0x8c:
			instruction_sty(addr_absolute());
		break;

		case 0x8d:
			instruction_sta(addr_absolute());
		break;

		case 0x8e:
			instruction_stx(addr_absolute());
		break;

		case 0x90:
			instruction_bcc(addr_relative());
		break;

		case 0x91:
			instruction_sta(addr_indirect_y());
		break;

		case 0x94:
			instruction_sty(addr_zero_page_x());
		break;

		case 0x95:
			instruction_sta(addr_zero_page_x());
		break;

		case 0x96:
			instruction_stx(addr_zero_page_y());
		break;

		case 0x98:
			instruction_tya();
		break;

		case 0x99:
			instruction_sta(addr_absolute_y());
		break;

		case 0x9a:
			instruction_txs();
		break;

		case 0x9d:
			instruction_sta(addr_absolute_x());
		break;

		case 0xa0:
			instruction_bcs(data_immediate());
		break;

		case 0xa1:
			instruction_lda(data_indirect_x());
		break;

		case 0xa2:
			instruction_ldx(data_immediate());
		break;

		case 0xa4:
			instruction_bcs(data_zero_page());
		break;

		case 0xa5:
			instruction_lda(data_zero_page());
		break;

		case 0xa6:
			instruction_ldx(data_zero_page());
		break;

		case 0xa8:
			instruction_tay();
		break;

		case 0xa9:
			instruction_lda(data_immediate());
		break;

		case 0xaa:
			instruction_tax();
		break;

		case 0xac:
			instruction_bcs(data_absolute());
		break;

		case 0xad:
			instruction_lda(data_absolute());
		break;

		case 0xae:
			instruction_ldx(data_absolute());
		break;

		case 0xb0:
			instruction_bcs(addr_relative());
		break;

		case 0xb1:
			instruction_lda(data_indirect_y());
		break;

		case 0xb4:
			instruction_bcs(data_zero_page_x());
		break;

		case 0xb5:
			instruction_lda(data_zero_page_x());
		break;

		case 0xb6:
			instruction_ldx(data_zero_page_y());
		break;

		case 0xb8:
			instruction_clv();
		break;

		case 0xb9:
			instruction_lda(data_absolute_y());
		break;

		case 0xba:
			instruction_tsx();
		break;

		case 0xbc:
			instruction_bcs(data_absolute_x());
		break;

		case 0xbd:
			instruction_lda(data_absolute_x());
		break;

		case 0xbe:
			instruction_ldx(data_absolute_y());
		break;

		case 0xc0:
			instruction_cpy(data_immediate());
		break;

		case 0xc1:
			instruction_cmp(data_indirect_x());
		break;

		case 0xc4:
			instruction_cpy(data_zero_page());
		break;

		case 0xc5:
			instruction_cmp(data_zero_page());
		break;

		case 0xc6:
			instruction_dec(addr_zero_page());
		break;

		case 0xc8:
			instruction_iny();
		break;

		case 0xc9:
			instruction_cmp(data_immediate());
		break;

		case 0xca:
			instruction_dex();
		break;

		case 0xcc:
			instruction_cpy(data_absolute());
		break;

		case 0xcd:
			instruction_cmp(data_absolute());
		break;

		case 0xce:
			instruction_dec(addr_absolute());
		break;

		case 0xd0:
			instruction_bne(addr_relative());
		break;

		case 0xd1:
			instruction_cmp(data_indirect_y());
		break;

		case 0xd5:
			instruction_cmp(data_zero_page_x());
		break;

		case 0xd6:
			instruction_dec(addr_zero_page_x());
		break;

		case 0xd8:
			instruction_cld();
		break;

		case 0xd9:
			instruction_cmp(data_absolute_y());
		break;

		case 0xdd:
			instruction_cmp(data_absolute_x());
		break;

		case 0xde:
			instruction_dec(addr_absolute_x());
		break;

		case 0xe0:
			instruction_cpx(data_immediate());
		break;

		case 0xe1:
			instruction_sbc(data_indirect_x());
		break;

		case 0xe4:
			instruction_cpx(data_zero_page());
		break;

		case 0xe5:
			instruction_sbc(data_zero_page());
		break;

		case 0xe6:
			instruction_inc(addr_zero_page());
		break;

		case 0xe8:
			instruction_inx();
		break;

		case 0xe9:
			instruction_sbc(data_immediate());
		break;

		case 0xea:
			instruction_nop();
		break;

		case 0xec:
			instruction_cpx(data_absolute());
		break;

		case 0xed:
			instruction_sbc(data_absolute());
		break;

		case 0xee:
			instruction_inc(addr_absolute());
		break;

		case 0xf0:
			instruction_beq(addr_relative());
		break;

		case 0xf1:
			instruction_sbc(data_indirect_y());
		break;

		case 0xf5:
			instruction_sbc(data_zero_page_x());
		break;

		case 0xf6:
			instruction_inc(addr_zero_page_x());
		break;

		case 0xf8:
			instruction_sed();
		break;

		case 0xf9:
			instruction_sbc(data_absolute_y());
		break;

		case 0xfd:
			instruction_sbc(data_absolute_x());
		break;

		case 0xfe:
			instruction_inc(addr_absolute_x());
		break;

		default:
			not_implemented();
		break;
	}
}

void fetch() {
	instruction = memory[program_counter++];
	printf("%x\n", instruction);
}

unsigned char flag_break() {
	return processor_status_flag(STATUS_BIT_BREAK);
}

void flag_break_set() {
	processor_status_flag_set(STATUS_BIT_BREAK);
}

void flag_break_unset() {
	processor_status_flag_unset(STATUS_BIT_BREAK);
}

unsigned char flag_carry() {
	return processor_status_flag(STATUS_BIT_CARRY);
}

void flag_carry_set() {
	processor_status_flag_set(STATUS_BIT_CARRY);
}

void flag_carry_unset() {
	processor_status_flag_unset(STATUS_BIT_CARRY);
}

unsigned char flag_decimal() {
	return processor_status_flag(STATUS_BIT_DECIMAL);
}

void flag_decimal_set() {
	processor_status_flag_set(STATUS_BIT_DECIMAL);
}

void flag_decimal_unset() {
	processor_status_flag_unset(STATUS_BIT_DECIMAL);
}

unsigned char flag_interrupt() {
	return processor_status_flag(STATUS_BIT_INTERRUPT);
}

void flag_interrupt_set() {
	processor_status_flag_set(STATUS_BIT_INTERRUPT);
}

void flag_interrupt_unset() {
	processor_status_flag_unset(STATUS_BIT_INTERRUPT);
}

unsigned char flag_negative() {
	return processor_status_flag(STATUS_BIT_NEGATIVE);
}

void flag_negative_set() {
	processor_status_flag_set(STATUS_BIT_NEGATIVE);
}

void flag_negative_unset() {
	processor_status_flag_unset(STATUS_BIT_NEGATIVE);
}

unsigned char flag_overflow() {
	return processor_status_flag(STATUS_BIT_OVERFLOW);
}

void flag_overflow_set() {
	processor_status_flag_set(STATUS_BIT_OVERFLOW);
}

void flag_overflow_unset() {
	processor_status_flag_unset(STATUS_BIT_OVERFLOW);
}

unsigned char flag_zero() {
	return processor_status_flag(STATUS_BIT_ZERO);
}

void flag_zero_set() {
	processor_status_flag_set(STATUS_BIT_ZERO);
}

void flag_zero_unset() {
	processor_status_flag_unset(STATUS_BIT_ZERO);
}

void instruction_adc(unsigned char data) {
	/*
	ADC - Add with Carry

	A,Z,C,N = A+M+C
	This instruction adds the contents of a memory location to the accumulator together with the carry bit.
	If overflow occurs the carry bit is set, this enables multiple byte addition to be performed.
	*/
	unsigned short sum = accumulator + data + flag_carry();
	accumulator = sum;
	check_negative(accumulator);
	check_overflow(sum);
	check_zero(accumulator);
}

void instruction_and(unsigned char data) {
	/*
	AND - Logical AND

	A,Z,N = A&M
	A logical AND is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
	*/
	accumulator &= data;
	check_negative(data);
	check_zero(data);
}

void instruction_asl(unsigned short addr) {
	/*
	ASL - Arithmetic Shift Left

	A,Z,C,N = M*2 or M,Z,C,N = M*2
	This operation shifts all the bits of the accumulator or memory contents one bit left.
	Bit 0 is set to 0 and bit 7 is placed in the carry flag.
	The effect of this operation is to multiply the memory contents by 2 (ignoring 2's complement considerations), setting the carry if the result will not fit in 8 bits.
	*/
	unsigned char *target;

	if (addr) {
		target = &memory[addr];
	} else {
		target = &accumulator;
	}

	if (*target >> 7) {
		flag_carry_set();
	} else {
		flag_carry_unset();
	}

	*target <<= 1;
	check_negative(*target);
	check_zero(*target);
}

void instruction_bcc(signed char addr) {
	/*
	BCC - Branch if Carry Clear

	If the carry flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
	*/
	if (!flag_carry()) {
		program_counter += addr;
	}
}

void instruction_bcs(signed char addr) {
	/*
	BCS - Branch if Carry Set

	If the carry flag is set then add the relative displacement to the program counter to cause a branch to a new location.
	*/
	if (flag_carry()) {
		program_counter += addr;
	}
}

void instruction_beq(signed char addr) {
	/*
	BEQ - Branch if Equal

	If the zero flag is set then add the relative displacement to the program counter to cause a branch to a new location.
	*/
	if (flag_zero()) {
		program_counter += addr;
	}
}

void instruction_bit(unsigned char data) {
	/*
	BIT - Bit Test

	A & M, N = M7, V = M6
	This instructions is used to test if one or more bits are set in a target memory location.
	The mask pattern in A is ANDed with the value in memory to set or clear the zero flag, but the result is not kept.
	Bits 7 and 6 of the value from memory are copied into the N and V flags.
	*/
	unsigned char result = accumulator & data;

	if ((result >> STATUS_BIT_NEGATIVE) & 1) {
		flag_negative_set();
	} else {
		flag_negative_unset();
	}

	if ((result >> STATUS_BIT_OVERFLOW) & 1) {
		flag_overflow_set();
	} else {
		flag_overflow_unset();
	}
}

void instruction_bmi(signed char addr) {
	/*
	BMI - Branch if Minus

	If the negative flag is set then add the relative displacement to the program counter to cause a branch to a new location.
	*/
	if (flag_negative()) {
		program_counter += addr;
	}
}

void instruction_bne(signed char addr) {
	/*
	BNE - Branch if Not Equal

	If the zero flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
	*/
	if (!flag_negative()) {
		program_counter += addr;
	}
}

void instruction_bpl(signed char addr) {
	/*
	BPL - Branch if Positive

	If the negative flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
	*/
	if (flag_negative()) {
		program_counter += addr;
	}
}

void instruction_brk() {
	/*
	BRK - Force Interrupt

	The BRK instruction forces the generation of an interrupt request.
	The program counter and processor status are pushed on the stack then the IRQ interrupt vector at $FFFE/F is loaded into the PC and the break flag in the status set to one.
	*/
	stack_push(program_counter);
	stack_push(program_counter >> BYTE_SIZE);
	stack_push(processor_status);
	program_counter = memory[0xfffe] | (memory[0xffff] << BYTE_SIZE);
	flag_break_set();
}

void instruction_bvc(signed char addr) {
	/*
	BVC - Branch if Overflow Clear

	If the overflow flag is clear then add the relative displacement to the program counter to cause a branch to a new location.
	*/
	if (!flag_overflow()) {
		program_counter += addr;
	}
}

void instruction_bvs(signed char addr) {
	/*
	BVS - Branch if Overflow Set

	If the overflow flag is set then add the relative displacement to the program counter to cause a branch to a new location.
	*/
	if (flag_overflow()) {
		program_counter += addr;
	}
}

void instruction_clc() {
	/*
	CLC - Clear Carry Flag

	C = 0
	Set the carry flag to zero.
	*/
	flag_carry_unset();
}

void instruction_cld() {
	/*
	CLD - Clear Decimal Mode

	D = 0
	Sets the decimal mode flag to zero.
	*/
	flag_decimal_unset();
}

void instruction_cli() {
	/*
	CLI - Clear Interrupt Disable

	I = 0
	Clears the interrupt disable flag allowing normal interrupt requests to be serviced.
	*/
	flag_interrupt_unset();
}

void instruction_clv() {
	/*
	CLV - Clear Overflow Flag

	V = 0
	Clears the overflow flag.
	*/
	flag_overflow_unset();
}

void instruction_cmp(unsigned char data) {
	/*
	CMP - Compare

	Z,C,N = A-M
	This instruction compares the contents of the accumulator with another memory held value and sets the zero and carry flags as appropriate.
	*/
	unsigned char result = accumulator - data;
	check_negative(result);
	check_zero(result);

	if (accumulator < data) {
		flag_carry_unset();
	} else {
		flag_carry_set();
	}
}

void instruction_cpx(unsigned char data) {
	/*
	CPX - Compare X Register

	Z,C,N = X-M
	This instruction compares the contents of the X register with another memory held value and sets the zero and carry flags as appropriate.
	*/
	unsigned char result = index_register_x - data;
	check_negative(result);
	check_zero(result);

	if (index_register_x < data) {
		flag_carry_unset();
	} else {
		flag_carry_set();
	}
}

void instruction_cpy(unsigned char data) {
	/*
	CPY - Compare Y Register

	Z,C,N = Y-M
	This instruction compares the contents of the Y register with another memory held value and sets the zero and carry flags as appropriate.
	*/
	unsigned char result = index_register_y - data;
	check_negative(result);
	check_zero(result);

	if (index_register_y < data) {
		flag_carry_unset();
	} else {
		flag_carry_set();
	}
}

void instruction_dec(unsigned short addr) {
	/*
	DEC - Decrement Memory

	M,Z,N = M-1
	Subtracts one from the value held at a specified memory location setting the zero and negative flags as appropriate.
	*/
	--memory[addr];
	check_negative(memory[addr]);
	check_zero(memory[addr]);
}

void instruction_dex() {
	/*
	DEX - Decrement X Register

	X,Z,N = X-1
	Subtracts one from the X register setting the zero and negative flags as appropriate.
	*/
	--index_register_x;
	check_negative(index_register_x);
	check_zero(index_register_x);
}

void instruction_dey() {
	/*
	DEY - Decrement Y Register

	Y,Z,N = Y-1
	Subtracts one from the Y register setting the zero and negative flags as appropriate.
	*/
	--index_register_y;
	check_negative(index_register_y);
	check_zero(index_register_y);
}

void instruction_eor(unsigned char data) {
	/*
	EOR - Exclusive OR

	A,Z,N = A^M
	An exclusive OR is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
	*/
	accumulator ^= data;
	check_negative(accumulator);
	check_zero(accumulator);
}

void instruction_inc(unsigned short addr) {
	/*
	INC - Increment Memory

	M,Z,N = M+1
	Adds one to the value held at a specified memory location setting the zero and negative flags as appropriate.
	*/
	++memory[addr];
	check_negative(memory[addr]);
	check_zero(memory[addr]);
}

void instruction_inx() {
	/*
	INX - Increment X Register

	X,Z,N = X+1
	Adds one to the X register setting the zero and negative flags as appropriate.
	*/
	++index_register_x;
	check_negative(index_register_x);
	check_zero(index_register_x);
}

void instruction_iny() {
	/*
	INY - Increment Y Register

	Y,Z,N = Y+1
	Adds one to the Y register setting the zero and negative flags as appropriate.
	*/
	++index_register_y;
	check_negative(index_register_y);
	check_zero(index_register_y);
}

void instruction_jmp(unsigned short addr) {
	/*
	JMP - Jump

	Sets the program counter to the address specified by the operand.
	*/
	program_counter = addr;
}

void instruction_jsr(unsigned short addr) {
	/*
	JSR - Jump to Subroutine

	The JSR instruction pushes the address (minus one) of the return point on to the stack and then sets the program counter to the target memory address.
	*/
	stack_push(program_counter);
	stack_push(program_counter >> BYTE_SIZE);
	program_counter = addr;
}

void instruction_lda(unsigned char data) {
	/*
	LDA - Load Accumulator

	A,Z,N = M
	Loads a byte of memory into the accumulator setting the zero and negative flags as appropriate.
	*/
	accumulator = data;
	check_negative(accumulator);
	check_zero(accumulator);
}

void instruction_ldx(unsigned char data) {
	/*
	LDX - Load X Register

	X,Z,N = M
	Loads a byte of memory into the X register setting the zero and negative flags as appropriate.
	*/
	index_register_x = data;
	check_negative(index_register_x);
	check_zero(index_register_x);
}

void instruction_ldy(unsigned char data) {
	/*
	LDY - Load Y Register

	Y,Z,N = M
	Loads a byte of memory into the Y register setting the zero and negative flags as appropriate.
	*/
	index_register_y = data;
	check_negative(index_register_y);
	check_zero(index_register_y);
}

void instruction_lsr(unsigned short addr) {
	/*
	LSR - Logical Shift Right

	A,C,Z,N = A/2 or M,C,Z,N = M/2
	Each of the bits in A or M is shift one place to the right. The bit that was in bit 0 is shifted into the carry flag. Bit 7 is set to zero.
	*/
	unsigned char *target;

	if (addr) {
		target = &memory[addr];
	} else {
		target = &accumulator;
	}

	if (*target & 1) {
		flag_carry_set();
	} else {
		flag_carry_unset();
	}

	*target >>= 1;
	check_negative(*target);
	check_zero(*target);
}

void instruction_nop() {
	/*
	NOP - No Operation

	The NOP instruction causes no changes to the processor other than the normal incrementing of the program counter to the next instruction.
	*/
}

void instruction_ora(unsigned char data) {
	/*
	ORA - Logical Inclusive OR

	A,Z,N = A|M
	An inclusive OR is performed, bit by bit, on the accumulator contents using the contents of a byte of memory.
	*/
	accumulator |= data;
	check_negative(accumulator);
	check_zero(accumulator);
}

void instruction_pha() {
	/*
	PHA - Push Accumulator

	Pushes a copy of the accumulator on to the stack.
	*/
	stack_push(accumulator);
}

void instruction_php() {
	/*
	PHP - Push Processor Status

	Pushes a copy of the status flags on to the stack.
	*/
	stack_push(processor_status);
}

void instruction_pla() {
	/*
	PLA - Pull Accumulator

	Pulls an 8 bit value from the stack and into the accumulator.
	The zero and negative flags are set as appropriate.
	*/
	accumulator = stack_pull();
	check_negative(accumulator);
	check_zero(accumulator);
}

void instruction_plp() {
	/*
	PLP - Pull Processor Status

	Pulls an 8 bit value from the stack and into the processor flags.
	The flags will take on new states as determined by the value pulled.
	*/
	processor_status = stack_pull();
}

void instruction_rol(unsigned short addr) {
	/*
	ROL - Rotate Left

	Move each of the bits in either A or M one place to the left.
	Bit 0 is filled with the current value of the carry flag whilst the old bit 7 becomes the new carry flag value.
	*/
	unsigned char carry;
	unsigned char *target;

	carry = flag_carry();

	if (addr) {
		target = &memory[addr];
	} else {
		target = &accumulator;
	}

	if (*target >> 7) {
		flag_carry_set();
	} else {
		flag_carry_unset();
	}

	*target <<= 1;
	*target |= carry;
	check_negative(*target);
	check_zero(*target);
}

void instruction_ror(unsigned short addr) {
	/*
	ROR - Rotate Right

	Move each of the bits in either A or M one place to the right.
	Bit 7 is filled with the current value of the carry flag whilst the old bit 0 becomes the new carry flag value.
	*/
	unsigned char carry;
	unsigned char *target;

	carry = flag_carry();

	if (addr) {
		target = &memory[addr];
	} else {
		target = &accumulator;
	}

	if (*target & 1) {
		flag_carry_set();
	} else {
		flag_carry_unset();
	}

	*target >>= 1;
	*target |= carry << 7;
	check_negative(*target);
	check_zero(*target);
}

void instruction_rti() {
	/*
	RTI - Return from Interrupt

	The RTI instruction is used at the end of an interrupt processing routine.
	It pulls the processor flags from the stack followed by the program counter.
	*/
	processor_status = stack_pull();
	program_counter = stack_pull() << BYTE_SIZE;
	program_counter |= stack_pull();
}

void instruction_rts() {
	/*
	RTS - Return from Subroutine

	The RTS instruction is used at the end of a subroutine to return to the calling routine.
	It pulls the program counter (minus one) from the stack.
	*/
	program_counter = stack_pull() << BYTE_SIZE;
	program_counter |= stack_pull();
}

void instruction_sbc(unsigned char data) {
	/*
	SBC - Subtract with Carry

	A,Z,C,N = A-M-(1-C)
	This instruction subtracts the contents of a memory location to the accumulator together with the not of the carry bit.
	If overflow occurs the carry bit is clear, this enables multiple byte subtraction to be performed.
	*/
	unsigned char subtract = data;

	if (!flag_carry()) {
		++subtract;
	}

	if (accumulator < subtract) {
		flag_carry_unset();
	} else {
		flag_carry_set();
	}

	accumulator -= subtract;
	check_negative(accumulator);
	check_zero(accumulator);
}

void instruction_sec() {
	/*
	SEC - Set Carry Flag

	C = 1
	Set the carry flag to one.
	*/
	flag_carry_set();
}

void instruction_sed() {
	/*
	SED - Set Decimal Flag

	D = 1
	Set the decimal mode flag to one.
	*/
	flag_decimal_set();
}

void instruction_sei() {
	/*
	SEI - Set Interrupt Disable

	I = 1
	Set the interrupt disable flag to one.
	*/
	flag_interrupt_set();
}

void instruction_sta(unsigned short addr) {
	/*
	STA - Store Accumulator

	M = A
	Stores the contents of the accumulator into memory.
	*/
	memory[addr] = accumulator;
}

void instruction_stx(unsigned short addr) {
	/*
	STX - Store X Register

	M = X
	Stores the contents of the X register into memory.
	*/
	memory[addr] = index_register_x;
}

void instruction_sty(unsigned short addr) {
	/*
	STY - Store Y Register

	M = Y
	Stores the contents of the Y register into memory.
	*/
	memory[addr] = index_register_y;
}

void instruction_tax() {
	/*
	TAX - Transfer Accumulator to X

	X = A
	Copies the current contents of the accumulator into the X register and sets the zero and negative flags as appropriate.
	*/
	index_register_x = accumulator;
	check_negative(index_register_x);
	check_zero(index_register_x);
}

void instruction_tay() {
	/*
	TAY - Transfer Accumulator to Y

	Y = A
	Copies the current contents of the accumulator into the Y register and sets the zero and negative flags as appropriate.
	*/
	index_register_y = accumulator;
	check_negative(index_register_y);
	check_zero(index_register_y);
}

void instruction_tsx() {
	/*
	TSX - Transfer Stack Pointer to X

	X = S
	Copies the current contents of the stack register into the X register and sets the zero and negative flags as appropriate.
	*/
	index_register_x = stack_pointer;
	check_negative(index_register_x);
	check_zero(index_register_x);
}

void instruction_txa() {
	/*
	TXA - Transfer X to Accumulator

	A = X
	Copies the current contents of the X register into the accumulator and sets the zero and negative flags as appropriate.
	*/
	accumulator = index_register_x;
	check_negative(accumulator);
	check_zero(accumulator);
}

void instruction_txs() {
	/*
	TXS - Transfer X to Stack Pointer

	S = X
	Copies the current contents of the X register into the stack register.
	*/
	stack_pointer = index_register_x;
}

void instruction_tya() {
	/*
	TYA - Transfer Y to Accumulator

	A = Y
	Copies the current contents of the Y register into the accumulator and sets the zero and negative flags as appropriate.
	*/
	accumulator = index_register_y;
	check_negative(accumulator);
	check_zero(accumulator);
}

void load_rom(char path[]) {
	FILE *file;
	unsigned char byte;
	unsigned short i;

	file = fopen(path, "rb");

	if (!file) {
		printf("Can't open %s!\n", path);
		exit(1);
	}

	printf("Loading %s...\n", path);
	i = ROM_ADDRESS;

	while (!feof(file)) {
		fread(&byte, 1, 1, file);
		memory[i++] = byte;
	}

	fclose(file);
	program_counter = (memory[0x1ffc] | (memory[0x1ffd] << BYTE_SIZE)) & 0x1fff;
	clrscr();
	cycle();
}

int main(int argc, char *argv[]) {
	if (argc < 2) {
		printf("Usage: %s file.rom [frequency]\n", argv[0]);
		exit(1);
	} else {
		if (argc > 2) {
			frequency = atoi(argv[2]);
		}

		load_rom(argv[1]);
		clrscr();
	}

	return 0;
}

void not_implemented() {
	running = 0;
	printf("Instruction %x not implemented.\n", instruction);
	exit(1);
}

unsigned short operand_2bytes() {
	unsigned char byte1;
	unsigned char byte2;

	fetch();
	byte1 = instruction;
	fetch();
	byte2 = instruction;
	return byte1 | (byte2 << BYTE_SIZE);
}

unsigned char processor_status_flag(unsigned char flag_bit) {
	return (processor_status >> flag_bit) & 1;
}

void processor_status_flag_set(unsigned char flag_bit) {
	processor_status |= (1 << flag_bit);
}

void processor_status_flag_unset(unsigned char flag_bit) {
	processor_status &= ~(1 << flag_bit);
}

unsigned short stack_pull() {
	return stack[++stack_pointer];
}

void stack_push(unsigned short data) {
	stack[stack_pointer--] = data;
}

void update() {
	if (kbhit() && getch() == KEY_ESC) {
		running = 0;
	}
}
