#include <stdio.h>

#define BYTE_SIZE 8
#define KEY_ESC 27
#define MEMORY_SIZE 0x2000
/*
0x0000 - 0x007f: TIA
0x0080 - 0x00ff: RAM (128 bytes)
0x0200 - 0x02ff: RIOT
0x1000 - 0x1fff: ROM
*/
#define MILLISECONDS 1000
#define REGISTER_SIZE 0x10
#define ROM_ADDRESS 0x1000
#define STACK_SIZE 0x100
#define STATUS_BIT_BREAK 4
#define STATUS_BIT_CARRY 0
#define STATUS_BIT_DECIMAL 3
#define STATUS_BIT_INTERRUPT 2
#define STATUS_BIT_NEGATIVE 7
#define STATUS_BIT_OVERFLOW 6
#define STATUS_BIT_ZERO 1

unsigned char accumulator;
unsigned char frequency = 0;
unsigned char index_register_x;
unsigned char index_register_y;
unsigned char instruction;
unsigned char memory[MEMORY_SIZE];
unsigned char processor_status = 36;
unsigned short program_counter;
unsigned char registers[REGISTER_SIZE];
unsigned char running = 1;
unsigned short stack[STACK_SIZE];
unsigned short stack_pointer = 0xff;

unsigned short addr_indirect_x();
unsigned short addr_indirect_y();
signed char addr_relative();
void check_negative();
void check_overflow();
void check_zero();
void cycle();
unsigned char data_indirect_x();
unsigned char data_indirect_y();
unsigned char data_zero_page();
unsigned char data_zero_page_x();
unsigned char data_zero_page_y();
void decode();
void fetch();
unsigned char flag_decimal();
void flag_decimal_set();
void flag_decimal_unset();
unsigned char flag_negative();
void flag_negative_set();
void flag_negative_unset();
unsigned char flag_overflow();
void flag_overflow_set();
void flag_overflow_unset();
unsigned char flag_zero();
void flag_zero_set();
void flag_zero_unset();
unsigned char get_key();
void instruction_adc(unsigned char data);
void instruction_and(unsigned char data);
void instruction_asl(unsigned short addr);
void instruction_bcc(signed char addr);
void instruction_bcs(signed char addr);
void instruction_beq(signed char addr);
void instruction_bit(unsigned char data);
void instruction_bmi(signed char addr);
void instruction_bne(signed char addr);
void instruction_bpl(signed char addr);
void instruction_brk();
void instruction_bvc(signed char addr);
void instruction_bvs(signed char addr);
void instruction_clc();
void instruction_cld();
void instruction_cli();
void instruction_clv();
void instruction_cmp(unsigned char data);
void instruction_cpx(unsigned char data);
void instruction_cpy(unsigned char data);
void instruction_dec(unsigned short addr);
void instruction_dex();
void instruction_dey();
void instruction_eor(unsigned char data);
void instruction_inc(unsigned short addr);
void instruction_inx();
void instruction_iny();
void instruction_jmp(unsigned short addr);
void instruction_jsr(unsigned short addr);
void instruction_lda(unsigned char data);
void instruction_ldx(unsigned char data);
void instruction_ldy(unsigned char data);
void instruction_lsr(unsigned short addr);
void instruction_nop();
void instruction_ora(unsigned char data);
void instruction_pha();
void instruction_php();
void instruction_pla();
void instruction_plp();
void instruction_rol(unsigned short addr);
void instruction_ror(unsigned short addr);
void instruction_rti();
void instruction_rts();
void instruction_sbc(unsigned char data);
void instruction_sec();
void instruction_sed();
void instruction_sei();
void instruction_sta(unsigned short addr);
void instruction_stx(unsigned short addr);
void instruction_sty(unsigned short addr);
void instruction_tax();
void instruction_tay();
void instruction_tsx();
void instruction_txa();
void instruction_txs();
void instruction_tya();
void load_rom(char path[]);
void not_implemented();
unsigned short operand_2bytes();
unsigned char processor_status_flag(unsigned char flag_bit);
void processor_status_flag_set(unsigned char flag_bit);
void processor_status_flag_unset(unsigned char flag_bit);
unsigned short stack_pull();
void stack_push(unsigned short data);
void update();
