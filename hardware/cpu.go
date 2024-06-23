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
	WRAM []uint8
	Bus
	Table [256]func(*OpcodeInfo)
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
	//x0  x1    x2     x3     x4     x5     x6     x7     x8     x9     xA     xB     xC     xD     xE     xF
	mIMP, mIDX, mIMP,
}

func (c *CPU) clock() {

}
