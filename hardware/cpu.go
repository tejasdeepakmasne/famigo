package hardware

type OpcodeInfo struct {
	mode   AddressingMode
	name   string
	cycles int
}

// hardware constants
const (
	STACK_START uint16 = 0x1000
	STACK_TOP   uint8  = 0xFF
	NMI_ADDR    uint16 = 0xFFFA
	IRQ_ADDR    uint16 = 0xFFFE
	RESET_ADDR  uint16 = 0xFFFC
)

type CPU struct {
	PC   uint16
	SP   uint8
	A    uint8
	X    uint8
	Y    uint8
	P    uint8
	WRAM [0x10000]uint8
	Bus
	Table [256]func(*OpcodeInfo) uint64
	Cycle uint64
}

type Flag uint8

const (
	C Flag = 1
	Z Flag = 2
	I Flag = 4
	D Flag = 8
	B Flag = 16
	U Flag = 32
	V Flag = 64
	N Flag = 128
)

type AddressingMode string

const (
	mIMP AddressingMode = "IMP" //Implied
	mACC AddressingMode = "ACC" //Accumulator
	mZP0 AddressingMode = "ZP0" //Zero Page
	mZPX AddressingMode = "ZPX" //ZeroPageX
	mZPY AddressingMode = "ZPY" //ZeroPAgeY
	mABS AddressingMode = "ABS" //Absolute
	mABX AddressingMode = "ABX" //AbsoluteX
	mABY AddressingMode = "ABY" //AbsoluteY
	mREL AddressingMode = "REL" //Relative
	mIND AddressingMode = "IND" //Indirect
	mIDX AddressingMode = "IDX" //IndirectX
	mIDY AddressingMode = "IDY" //IndirectY
	mIMM AddressingMode = "IMM" //Immediate
)

func (c *CPU) Read16(address uint16) uint16 {
	hi := uint16(c.Read(address + 1))
	lo := uint16(c.Read(address))
	return hi<<8 | lo
}

func (c *CPU) Write16(address uint16, value uint16) {
	hi := value >> 8
	lo := value & 0b0000000011111111
	c.Write(address, uint8(lo))
	c.Write(address+1, uint8(hi))
}

func (c *CPU) setFLag(flag Flag, value uint8) {
	if value >= 1 {
		c.P = c.P | uint8(flag)
	} else {
		c.P = c.P & (^uint8(flag))
	}
}

func (c *CPU) getFlag(flag Flag) uint8 {
	if c.P&uint8(flag) == uint8(flag) {
		return 1
	} else {
		return 0
	}
}

func pagesDiffer(a, b uint16) bool {
	return a&0xFF00 != b&0xFF00
}

func (c *CPU) getModeInfo(mode AddressingMode) (uint16, uint8, bool) {
	var address uint16
	var operand uint8
	var pageCrossed bool
	switch mode {
	case mABS:
		address = c.Read16(c.PC)
		operand = c.Read(address)
	case mABX:
		address = c.Read16(c.PC) + uint16(c.Read(c.PC+2))
		operand = c.Read(address)
		pageCrossed = pagesDiffer(address, c.PC+2)
	case mABY:
		address = c.Read16(c.PC) + uint16(c.Read(c.PC+2))
		operand = c.Read(address)
		pageCrossed = pagesDiffer(address, c.PC+2)
	case mIMP:
		address = c.PC
		pageCrossed = false
	case mIND:
		if c.Read(c.PC) == 0xff {
			lo := c.Read(c.Read16(c.PC))
			MSB_addr := uint16(c.Read(c.PC+1)) << 8
			hi := c.Read(MSB_addr)
			address = uint16(hi)<<8 | uint16(lo)
			operand = c.Read(address)
		} else {
			temp := c.Read16(c.PC)
			address = c.Read16(temp)
			operand = c.Read(address)
		}
	case mACC:
		operand = c.A
		address = c.PC - 1
	case mIDX:
		baseAddr := c.Read(c.PC) + c.X
		address = c.Read16(uint16(baseAddr))
		operand = c.Read(address)
	case mIDY:
		address = c.Read16(uint16(c.Read(c.PC))) + uint16(c.Y)
		operand = c.Read(address)
		pageCrossed = pagesDiffer(address, c.PC)
	case mREL:
		if c.Read(c.PC)&0b10000000 == 0b10000000 {
			address = c.PC - uint16(c.Read(c.PC)) - 1
			operand = c.Read(c.PC)
		} else {
			address = c.PC + uint16(c.Read(c.PC)) - 1
			operand = c.Read(c.PC)
		}
	case mIMM:
		operand = c.Read(c.PC)
	}
	return address, operand, pageCrossed
}

var opcodeNameMatrix [256]string = [256]string{
	//x0    x1     x2     x3     x4     x5     x6     x7     x8     x9     xA     xB     xC     xD     xE     xF
	"BRK", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO", "PHP", "ORA", "ASL", "ANC", "NOP", "ORA", "ASL", "SLO", //0x
	"BPL", "ORA", "KIL", "SLO", "NOP", "ORA", "ASL", "SLO", "CLC", "ORA", "NOP", "SLO", "NOP", "ORA", "ASL", "SLO", //1x
	"JSR", "AND", "KIL", "RLA", "BIT", "AND", "ROL", "RLA", "PLP", "AND", "ROL", "ANC", "BIT", "AND", "ROL", "RLA", //2x
	"BMI", "AND", "KIL", "RLA", "NOP", "AND", "ROL", "RLA", "SEC", "AND", "NOP", "RLA", "NOP", "AND", "ROL", "RLA", //3x
	"RTI", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE", "PHA", "EOR", "LSR", "ALR", "JMP", "EOR", "LSR", "SRE", //4x
	"BVC", "EOR", "KIL", "SRE", "NOP", "EOR", "LSR", "SRE", "CLI", "EOR", "NOP", "SRE", "NOP", "EOR", "LSR", "SRE", //5x
	"RTS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA", "PLA", "ADC", "ROR", "ARR", "JMP", "ADC", "ROR", "RRA", //6x
	"BVS", "ADC", "KIL", "RRA", "NOP", "ADC", "ROR", "RRA", "SEI", "ADC", "NOP", "RRA", "NOP", "ADC", "ROR", "RRA", //7x
	"NOP", "STA", "NOP", "SAX", "STY", "STA", "STX", "SAX", "DEY", "NOP", "TXA", "XAA", "STY", "STA", "STX", "SAX", //8x
	"BCC", "STA", "KIL", "AHX", "STY", "STA", "STX", "SAX", "TYA", "STA", "TXS", "TAS", "SHY", "STA", "SHX", "AHX", //9x
	"LDY", "LDA", "LDX", "LAX", "LDY", "LDA", "LDX", "LAX", "TAY", "LDA", "TAX", "LAX", "LDY", "LDA", "LDX", "LAX", //Ax
	"BCS", "LDA", "KIL", "LAX", "LDY", "LDA", "LDX", "LAX", "CLV", "LDA", "TSX", "LAS", "LDY", "LDA", "LDX", "LAX", //Bx
	"CPY", "CMP", "NOP", "DCP", "CPY", "CMP", "DEC", "DCP", "INY", "CMP", "DEX", "AXS", "CPY", "CMP", "DEC", "DCP", //Cx
	"BNE", "CMP", "KIL", "DCP", "NOP", "CMP", "DEC", "DCP", "CLD", "CMP", "NOP", "DCP", "NOP", "CMP", "DEC", "DCP", //Dx
	"CPX", "SBC", "NOP", "ISC", "CPX", "SBC", "INC", "ISC", "INX", "SBC", "NOP", "SBC", "CPX", "SBC", "INC", "ISC", //Ex
	"BEQ", "SBC", "KIL", "ISC", "NOP", "SBC", "INC", "ISC", "SED", "SBC", "NOP", "ISC", "NOP", "SBC", "INC", "ISC", //Fx
}

var opcodeAddrMode [256]AddressingMode = [256]AddressingMode{
	//x0  x1    x2    x3    x4    x5    x6    x7    x8    x9    xA    xB    xC    xD    xE    xF
	mIMP, mIDX, mIMP, mIDX, mZP0, mZP0, mZP0, mZP0, mIMP, mIMM, mIMP, mIMM, mABS, mABS, mABS, mABS, //0x
	mREL, mIDY, mIMP, mIDY, mZPX, mZPX, mZPX, mZPX, mIMP, mABY, mIMP, mABY, mABX, mABX, mABX, mABX, //1x
	mABS, mIDX, mIMP, mIDX, mZP0, mZP0, mZP0, mZP0, mIMP, mIMM, mIMP, mIMM, mABS, mABS, mABS, mABS, //2x
	mREL, mIDY, mIMP, mIDY, mZPX, mZPX, mZPX, mZPX, mIMP, mABY, mIMP, mABY, mABX, mABX, mABX, mABX, //3x
	mIMP, mIDX, mIMP, mIDX, mZP0, mZP0, mZP0, mZP0, mIMP, mIMM, mIMP, mIMM, mABS, mABS, mABS, mABS, //4x
	mREL, mIDY, mIMP, mIDY, mZPX, mZPX, mZPX, mZPX, mIMP, mABY, mIMP, mABY, mABX, mABX, mABX, mABX, //5x
	mIMP, mIDX, mIMP, mIDX, mZP0, mZP0, mZP0, mZP0, mIMP, mIMM, mIMP, mIMM, mIND, mABS, mABS, mABS, //6x
	mREL, mIDY, mIMP, mIDY, mZPX, mZPX, mZPX, mZPX, mIMP, mABY, mIMP, mABY, mABX, mABX, mABX, mABX, //7x
	mIMM, mIDX, mIMP, mIDX, mZP0, mZP0, mZP0, mZP0, mIMP, mIMM, mIMP, mIMM, mABS, mABS, mABS, mABS, //8x
	mREL, mIDY, mIMP, mIDY, mZPX, mZPX, mZPY, mZPY, mIMP, mABY, mIMP, mABY, mABX, mABX, mABY, mABY, //9x
	mIMM, mIDX, mIMM, mIDX, mZP0, mZP0, mZP0, mZP0, mIMP, mIMM, mIMP, mIMM, mABS, mABS, mABS, mABS, //Ax
	mREL, mIDY, mIMP, mIDY, mZPX, mZPX, mZPX, mZPX, mIMP, mABY, mIMP, mABY, mABX, mABX, mABX, mABX, //Bx
	mIMM, mIDX, mIMP, mIDX, mZP0, mZP0, mZP0, mZP0, mIMP, mIMM, mIMP, mIMM, mABS, mABS, mABS, mABS, //Cx
	mREL, mIDY, mIMP, mIDY, mZPX, mZPX, mZPX, mZPX, mIMP, mABY, mIMP, mABY, mABX, mABX, mABX, mABX, //Dx
	mIMM, mIDX, mIMP, mIDX, mZP0, mZP0, mZP0, mZP0, mIMP, mIMM, mIMP, mIMM, mABS, mABS, mABS, mABS, //Ex
	mREL, mIDY, mIMP, mIDY, mZPX, mZPX, mZPX, mZPX, mIMP, mABY, mIMP, mABY, mABX, mABX, mABX, mABX, //Fx
}

var opcodeSize [256]uint16 = [256]uint16{
	0, 2, 0, 2, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	2, 2, 0, 2, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	0, 2, 0, 2, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	0, 2, 0, 2, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	1, 2, 1, 2, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 1, 0, 1, 2, 2, 2, 2,
	1, 2, 0, 2, 1, 1, 1, 1, 0, 2, 0, 2, 2, 2, 2, 2,
}

// instructionCycles indicates the number of cycles used by each instruction,
// not including conditional cycles
var opcodeCycles = [256]int{
	7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
	2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,
	2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
	2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,
	2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
	2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
	2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
}

func (c *CPU) createTable() {
	c.Table = [256]func(*OpcodeInfo) uint64{
		c.brk, c.ora, c.kil, c.slo, c.nop, c.ora, c.asl, c.slo,
		c.php, c.ora, c.asl, c.anc, c.nop, c.ora, c.asl, c.slo,
		c.bpl, c.ora, c.kil, c.slo, c.nop, c.ora, c.asl, c.slo,
		c.clc, c.ora, c.nop, c.slo, c.nop, c.ora, c.asl, c.slo,
		c.jsr, c.and, c.kil, c.rla, c.bit, c.and, c.rol, c.rla,
		c.plp, c.and, c.rol, c.anc, c.bit, c.and, c.rol, c.rla,
		c.bmi, c.and, c.kil, c.rla, c.nop, c.and, c.rol, c.rla,
		c.sec, c.and, c.nop, c.rla, c.nop, c.and, c.rol, c.rla,
		c.rti, c.eor, c.kil, c.sre, c.nop, c.eor, c.lsr, c.sre,
		c.pha, c.eor, c.lsr, c.alr, c.jmp, c.eor, c.lsr, c.sre,
		c.bvc, c.eor, c.kil, c.sre, c.nop, c.eor, c.lsr, c.sre,
		c.cli, c.eor, c.nop, c.sre, c.nop, c.eor, c.lsr, c.sre,
		c.rts, c.adc, c.kil, c.rra, c.nop, c.adc, c.ror, c.rra,
		c.pla, c.adc, c.ror, c.arr, c.jmp, c.adc, c.ror, c.rra,
		c.bvs, c.adc, c.kil, c.rra, c.nop, c.adc, c.ror, c.rra,
		c.sei, c.adc, c.nop, c.rra, c.nop, c.adc, c.ror, c.rra,
		c.nop, c.sta, c.nop, c.sax, c.sty, c.sta, c.stx, c.sax,
		c.dey, c.nop, c.txa, c.xaa, c.sty, c.sta, c.stx, c.sax,
		c.bcc, c.sta, c.kil, c.ahx, c.sty, c.sta, c.stx, c.sax,
		c.tya, c.sta, c.txs, c.tas, c.shy, c.sta, c.shx, c.ahx,
		c.ldy, c.lda, c.ldx, c.lax, c.ldy, c.lda, c.ldx, c.lax,
		c.tay, c.lda, c.tax, c.lax, c.ldy, c.lda, c.ldx, c.lax,
		c.bcs, c.lda, c.kil, c.lax, c.ldy, c.lda, c.ldx, c.lax,
		c.clv, c.lda, c.tsx, c.las, c.ldy, c.lda, c.ldx, c.lax,
		c.cpy, c.cmp, c.nop, c.dcp, c.cpy, c.cmp, c.dec, c.dcp,
		c.iny, c.cmp, c.dex, c.axs, c.cpy, c.cmp, c.dec, c.dcp,
		c.bne, c.cmp, c.kil, c.dcp, c.nop, c.cmp, c.dec, c.dcp,
		c.cld, c.cmp, c.nop, c.dcp, c.nop, c.cmp, c.dec, c.dcp,
		c.cpx, c.sbc, c.nop, c.isc, c.cpx, c.sbc, c.inc, c.isc,
		c.inx, c.sbc, c.nop, c.sbc, c.cpx, c.sbc, c.inc, c.isc,
		c.beq, c.sbc, c.kil, c.isc, c.nop, c.sbc, c.inc, c.isc,
		c.sed, c.sbc, c.nop, c.isc, c.nop, c.sbc, c.inc, c.isc,
	}
}

func (c *CPU) executeOpcode(initCycles uint64) uint64 {
	opcode := c.Read(c.PC)
	c.PC++
	info := &OpcodeInfo{
		mode:   opcodeAddrMode[opcode],
		name:   opcodeNameMatrix[opcode],
		cycles: opcodeCycles[opcode],
	}
	additionalCycles := c.Table[opcode](info)
	c.PC += opcodeSize[opcode]
	return additionalCycles

}

func (c *CPU) clock() uint64 {
	Cycles := c.Cycle
	Cycles += c.executeOpcode(Cycles)
	return Cycles - c.Cycle
}

func (c *CPU) reset() {
	c.PC = c.Read16(RESET_ADDR)
	c.SP = STACK_TOP
	c.P |= uint8(U)
	c.Cycle = 7
}

func (c *CPU) interrupt() {
	if c.getFlag(I) == 0 {
		c.push16(c.PC)
		c.pushStatus()
		c.setFLag(I, 1)
		c.PC = c.Read16(IRQ_ADDR)
		c.Cycle = 7
	}
}

func (c *CPU) pushStatus() {
	c.push(c.P | uint8(B) | uint8(U))
}

func (c *CPU) push(value uint8) {
	c.Write(STACK_START|uint16(c.SP), value)
	c.SP--
}

func (c *CPU) push16(value uint16) {
	c.push(uint8(value >> 8))
	c.push(uint8(value & 0xFF))
}

func NewCPU() *CPU {
	cpu := &CPU{
		SP: 0xFD,
		P:  0x24,
	}
	return cpu
}

func doesCarry(val uint16) bool {
	if val > 255 {
		return true
	}
	return false
}

func updateZandN(val uint8) {
	if val == 0 {
		c.setFLag(Z, 1)
	} else {
		c.setFLag(Z, 0)
	}

	if val > 127 {
		c.setFLag(N, 1)
	} else {
		c.setFLag(N, 0)
	}
}

func doesOverflow(v1 uint8, v2 uint8, val uint16) bool {
	if v1 < 128 && v2 < 128 {
		if val > 127 {
			return true
		}
	}
	if v1 > 127 && v2 > 127 {
		if val-256 < 128 {
			return true
		}
	}
	return false
}

// opcodes
func (c *CPU) adc(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	var temp uint16 = uint16(operand) + uint16(c.A) + uint16(c.getFlag(C))

	if doesCarry(temp) {
		c.setFLag(C, 1)
	} else {
		c.setFLag(C, 0)
	}

	if doesOverflow(c.A, operand+uint8(c.getFlag(C)), temp) {
		c.setFLag(V, 1)
	} else {
		c.setFLag(V, 0)
	}

	c.A = uint8(temp % 256)
	updateZandN(c.A)
	var additionalCycles uint64 = 0
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

// func main() {
// 	cpu := NewCPU()

// 	cpu.WRAM[0x8000] = 0xA9 // LDA Immediate
// 	cpu.WRAM[0x8001] = 0x42 // Value to load
// 	cpu.WRAM[0x8002] = 0x85 // STA Zero Page
// 	cpu.WRAM[0x8003] = 0x10 // Address to store
// 	cpu.WRAM[0x8004] = 0x00 // BRK

// 	cpu.PC = 0x8000

// 	for !cpu.getFlag(B) {
// 		cpu.clock()
// 	}

// 	fmt.Printf("Value at 0x10: %X\n", cpu.WRAM[0x10])
// } For testing the CPU
