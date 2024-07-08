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
		pageCrossed = pagesDiffer(c.PC, address)
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

// size of opcodes-1
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

func (c *CPU) executeOpcode() uint64 {
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

func (c *CPU) CpuClock() uint64 {
	Cycles := c.Cycle
	Cycles += c.executeOpcode()
	return Cycles - c.Cycle
}

func (c *CPU) Reset() {
	c.PC = c.Read16(RESET_ADDR)
	c.SP = STACK_TOP
	c.P |= uint8(U)
	c.Cycle = 7
}

func (c *CPU) IRQ() {
	if c.getFlag(I) == 0 {
		c.push16(c.PC)
		c.pushStatus()
		c.setFLag(I, 1)
		c.PC = c.Read16(IRQ_ADDR)
		c.Cycle += 7
	}
}

func (c *CPU) NMI() {
	c.push16(c.PC)
	c.pushStatus()
	c.setFLag(I, 1)
	c.PC = c.Read16(NMI_ADDR)
	c.Cycle += 7
}

func (c *CPU) pushStatus() {
	c.push(c.P | uint8(B) | uint8(U))
}

func (c *CPU) push(value uint8) {
	c.Write(STACK_START|uint16(c.SP), value)
	c.SP--
}

func (c *CPU) pull() uint8 {
	val := c.Read(STACK_START | uint16(c.SP))
	c.SP++
	return val
}

func (c *CPU) pull16() uint16 {
	topAddr := STACK_START | uint16(c.SP)
	lo := uint16(c.Read(topAddr))
	hi := uint16(c.Read(topAddr + 1))
	c.SP += 2
	return (hi << 8) | lo
}

func (c *CPU) push16(value uint16) {
	c.push(uint8(value >> 8))
	c.push(uint8(value & 0xFF))
}

func NewCPU() CPU {
	var cpu CPU
	cpu.A, cpu.X, cpu.Y = 0, 0, 0
	cpu.PC = cpu.Read16(0xfffc)
	cpu.SP = 0xfd
	cpu.P = 4
	return cpu
}

func (c *CPU) updateZandN(val uint8) {
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

// opcodes
func (c *CPU) adc(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	a := c.A
	b := operand
	carry := c.getFlag(C)
	c.A = a + b + carry
	c.updateZandN(c.A)

	if uint(a)+uint(b)+uint(carry) > 0xFF {
		c.setFLag(C, 1)
	} else {
		c.setFLag(C, 0)
	}
	if (a^b)&0x80 == 0 && (a^c.A)&0x80 != 0 {
		c.setFLag(V, 1)
	} else {
		c.setFLag(V, 0)
	}

	var additionalCycles uint64 = 0
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) and(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	c.A = c.A & operand
	c.updateZandN(c.A)
	var additionalCycles uint64 = 0
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) asl(info *OpcodeInfo) uint64 {
	address, operand, _ := c.getModeInfo(info.mode)
	c.setFLag(C, operand>>7)
	operand = operand << 1
	if info.mode == mACC {
		c.A = operand
	} else {
		c.Write(address, operand)
	}
	c.updateZandN(operand)
	return 0
}

func (c *CPU) bcc(info *OpcodeInfo) uint64 {
	address, _, pageCrossed := c.getModeInfo(info.mode)
	var additionalCycles uint64
	if c.getFlag(C) == 0 {
		c.PC = address
		additionalCycles++
		if pageCrossed {
			additionalCycles++
		}
	}
	return additionalCycles
}

func (c *CPU) bcs(info *OpcodeInfo) uint64 {
	address, _, pagesCrossed := c.getModeInfo(info.mode)
	var additionalCycles uint64
	if c.getFlag(C) == 1 {
		c.PC = address
		additionalCycles++
		if pagesCrossed {
			additionalCycles++
		}
	}
	return additionalCycles
}

func (c *CPU) beq(info *OpcodeInfo) uint64 {
	address, _, pagesCrossed := c.getModeInfo(info.mode)
	var additionalCycles uint64
	if c.getFlag(Z) == 1 {
		c.PC = address
		additionalCycles++
		if pagesCrossed {
			additionalCycles++
		}
	}
	return additionalCycles
}

func (c *CPU) bit(info *OpcodeInfo) uint64 {
	_, operand, _ := c.getModeInfo(info.mode)
	temp := c.A & operand
	m7 := (operand & (1 << 7)) >> 7
	m6 := (operand & (1 << 6)) >> 6
	c.setFLag(N, m7)
	c.setFLag(V, m6)
	if temp == 0 {
		c.setFLag(C, 1)
	} else {
		c.setFLag(C, 0)
	}
	return 0
}

func (c *CPU) bmi(info *OpcodeInfo) uint64 {
	address, _, pageCrossed := c.getModeInfo(info.mode)
	var additionalCycles uint64
	if c.getFlag(N) == 1 {
		c.PC = address
		additionalCycles++
		if pageCrossed {
			additionalCycles++
		}
	}
	return additionalCycles
}

func (c *CPU) bne(info *OpcodeInfo) uint64 {
	address, _, pageCrossed := c.getModeInfo(info.mode)
	var additionalCycles uint64
	if c.getFlag(C) == 0 {
		c.PC = address
		additionalCycles++
		if pageCrossed {
			additionalCycles++
		}
	}
	return additionalCycles
}

func (c *CPU) bpl(info *OpcodeInfo) uint64 {
	address, _, pageCrossed := c.getModeInfo(info.mode)
	var additionalCycles uint64
	if c.getFlag(N) == 0 {
		c.PC = address
		additionalCycles++
		if pageCrossed {
			additionalCycles++
		}
	}
	return additionalCycles
}

// copy of c.IRQ() duplicated to prevent double counting of cycles
func (c *CPU) brk(info *OpcodeInfo) uint64 {
	if c.getFlag(I) == 0 {
		c.push16(c.PC)
		c.pushStatus()
		c.setFLag(I, 1)
		c.PC = c.Read16(IRQ_ADDR)
	}

	return 0
}

func (c *CPU) bvc(info *OpcodeInfo) uint64 {
	address, _, pageCrossed := c.getModeInfo(info.mode)
	var additionalCycles uint64
	if c.getFlag(V) == 0 {
		c.PC = address
		additionalCycles++
		if pageCrossed {
			additionalCycles++
		}

	}
	return additionalCycles
}

func (c *CPU) bvs(info *OpcodeInfo) uint64 {
	address, _, pageCrossed := c.getModeInfo(info.mode)
	var additionalCycles uint64
	if c.getFlag(V) == 1 {
		c.PC = address
		additionalCycles++
		if pageCrossed {
			additionalCycles++
		}

	}
	return additionalCycles
}

func (c *CPU) clc(info *OpcodeInfo) uint64 {
	c.setFLag(C, 0)
	return 0
}

func (c *CPU) cld(info *OpcodeInfo) uint64 {
	c.setFLag(D, 0)
	return 0
}

func (c *CPU) cli(info *OpcodeInfo) uint64 {
	c.setFLag(I, 0)
	return 0
}

func (c *CPU) clv(info *OpcodeInfo) uint64 {
	c.setFLag(V, 0)
	return 0
}

func (c *CPU) cmp(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	temp := c.A - operand
	if temp >= 0 {
		c.setFLag(C, 1)
	} else {
		c.setFLag(C, 0)
	}

	c.updateZandN(temp)
	var additionalCycles uint64
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) cpx(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	temp := c.X - operand
	if temp >= 0 {
		c.setFLag(C, 1)
	} else {
		c.setFLag(C, 0)
	}

	c.updateZandN(temp)
	var additionalCycles uint64
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) cpy(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	temp := c.Y - operand
	if temp >= 0 {
		c.setFLag(C, 1)
	} else {
		c.setFLag(C, 0)
	}

	c.updateZandN(temp)
	var additionalCycles uint64
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) dec(info *OpcodeInfo) uint64 {
	address, operand, _ := c.getModeInfo(info.mode)
	operand--
	c.Write(address, operand)
	c.updateZandN(operand)
	return 0
}

func (c *CPU) dex(info *OpcodeInfo) uint64 {
	c.X--
	c.updateZandN(c.X)
	return 0
}

func (c *CPU) dey(info *OpcodeInfo) uint64 {
	c.Y--
	c.updateZandN(c.Y)
	return 0
}

func (c *CPU) eor(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	c.A = c.A ^ operand
	c.updateZandN(c.A)
	var additionalCycles uint64
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) inc(info *OpcodeInfo) uint64 {
	address, operand, _ := c.getModeInfo(info.mode)
	operand++
	c.Write(address, operand)
	c.updateZandN(operand)
	return 0
}

func (c *CPU) inx(info *OpcodeInfo) uint64 {
	c.X++
	c.updateZandN(c.X)
	return 0
}

func (c *CPU) iny(info *OpcodeInfo) uint64 {
	c.Y++
	c.updateZandN(c.Y)
	return 0
}

func (c *CPU) jmp(info *OpcodeInfo) uint64 {
	address, _, _ := c.getModeInfo(info.mode)
	c.PC = address
	return 0
}

func (c *CPU) jsr(info *OpcodeInfo) uint64 {
	address, _, _ := c.getModeInfo(info.mode)
	c.push16(c.PC + 1)
	c.PC = address
	return 0
}

func (c *CPU) lda(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	c.A = operand
	c.updateZandN(c.A)
	var additionalCycles uint64
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) ldx(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	c.X = operand
	c.updateZandN(c.X)
	var additionalCycles uint64
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) ldy(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	c.Y = operand
	c.updateZandN(c.Y)
	var additionalCycles uint64
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) lsr(info *OpcodeInfo) uint64 {
	address, operand, _ := c.getModeInfo(info.mode)
	c.setFLag(C, (operand & 1))
	operand = operand >> 1
	if info.mode == mACC {
		c.A = operand
	} else {
		c.Write(address, operand)
	}
	c.updateZandN(operand)
	return 0
}

func (c *CPU) nop(info *OpcodeInfo) uint64 {
	_, _, pageCrossed := c.getModeInfo(info.mode)
	var additionalCycles uint64
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) ora(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	c.A = c.A | operand
	c.updateZandN(c.A)
	var additionalCycles uint64
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) pha(info *OpcodeInfo) uint64 {
	c.push(c.A)
	return 0
}

func (c *CPU) php(info *OpcodeInfo) uint64 {
	c.pushStatus()
	return 0
}

func (c *CPU) pla(info *OpcodeInfo) uint64 {
	c.A = c.pull()
	c.updateZandN(c.A)
	return 0
}

func (c *CPU) plp(info *OpcodeInfo) uint64 {
	c.P = c.pull()
	return 0
}

func (c *CPU) rol(info *OpcodeInfo) uint64 {
	address, operand, _ := c.getModeInfo(info.mode)
	carry := c.getFlag(C)
	c.setFLag(C, (operand&(1<<7))>>7)
	operand = operand << 1
	operand = operand + carry
	c.updateZandN(operand)
	if info.mode == mACC {
		c.A = operand
	} else {
		c.Write(address, operand)
	}
	return 0
}

func (c *CPU) ror(info *OpcodeInfo) uint64 {
	address, operand, _ := c.getModeInfo(info.mode)
	carry := c.getFlag(C)
	c.setFLag(C, operand&1)
	operand = (operand >> 1) + (carry << 7)
	c.updateZandN(operand)
	if info.mode == mACC {
		c.A = operand
	} else {
		c.Write(address, operand)
	}
	return 0
}

func (c *CPU) rti(info *OpcodeInfo) uint64 {
	c.P = c.pull()
	c.PC = c.pull16() + 1
	return 0
}

func (c *CPU) rts(info *OpcodeInfo) uint64 {
	c.PC = c.pull16() + 1
	return 0
}

func (c *CPU) sbc(info *OpcodeInfo) uint64 {
	_, operand, pageCrossed := c.getModeInfo(info.mode)
	a := c.A
	b := operand
	carry := c.getFlag(C)
	c.A = a - b - (1 - carry)
	c.updateZandN(c.A)
	if int(a)-int(b)-int(1-carry) >= 0 {
		c.setFLag(C, 1)
	} else {
		c.setFLag(C, 0)
	}
	if (a^b)&0x80 != 0 && (a^c.A)&0x80 != 0 {
		c.setFLag(V, 1)
	} else {
		c.setFLag(V, 0)
	}
	var additionalCycles uint64
	if pageCrossed {
		additionalCycles++
	}
	return additionalCycles
}

func (c *CPU) sec(info *OpcodeInfo) uint64 {
	c.setFLag(C, 1)
	return 0
}

func (c *CPU) sed(info *OpcodeInfo) uint64 {
	c.setFLag(D, 1)
	return 0
}

func (c *CPU) sei(info *OpcodeInfo) uint64 {
	c.setFLag(I, 1)
	return 0
}

func (c *CPU) sta(info *OpcodeInfo) uint64 {
	address, _, _ := c.getModeInfo(info.mode)
	c.Write(address, c.A)
	return 0
}

func (c *CPU) stx(info *OpcodeInfo) uint64 {
	address, _, _ := c.getModeInfo(info.mode)
	c.Write(address, c.X)
	return 0
}

func (c *CPU) sty(info *OpcodeInfo) uint64 {
	address, _, _ := c.getModeInfo(info.mode)
	c.Write(address, c.Y)
	return 0
}

func (c *CPU) tax(info *OpcodeInfo) uint64 {
	c.X = c.A
	c.updateZandN(c.X)
	return 0
}

func (c *CPU) tay(info *OpcodeInfo) uint64 {
	c.Y = c.A
	c.updateZandN(c.Y)
	return 0
}

func (c *CPU) tsx(info *OpcodeInfo) uint64 {
	c.X = c.SP
	c.updateZandN(c.X)
	return 0
}

func (c *CPU) txa(info *OpcodeInfo) uint64 {
	c.A = c.X
	c.updateZandN(c.A)
	return 0
}

func (c *CPU) txs(info *OpcodeInfo) uint64 {
	c.SP = c.X
	return 0
}

func (c *CPU) tya(info *OpcodeInfo) uint64 {
	c.A = c.Y
	c.updateZandN(c.A)
	return 0
}

// Illegal Opcodes
func (c *CPU) kil(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) slo(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) rla(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) sre(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) rra(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) sax(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) ahx(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) lax(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) dcp(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) isc(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) anc(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) alr(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) arr(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) xaa(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) tas(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) las(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) axs(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) shy(info *OpcodeInfo) uint64 {
	return 0
}

func (c *CPU) shx(info *OpcodeInfo) uint64 {
	return 0
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
