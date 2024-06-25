package hardware

import "fmt"

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

func ( c *CPU) setFLag(flag uint8, condition bool) {
	if condition {
		c.P |= flag
	} else {
		c.P &^= flag
	}
}

func (c *CPU) getFlag(flag uint8) bool {
	return c.P&flag != 0
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

func (c * CPU) executeOpcode() {
	opcode := c.Read(c.PC)
	c.PC++
	info := &OpcodeInfo{
		mode: opcodeAddrMode[opcode],
		name: opcodeNameMatrix[opcode],
		cycles: 0,
	}
	address, operand, pageCrossed := c.getModeInfo(info.mode)
	switch opcode {
	case 0x00:
		c.BRK(info, pageCrossed)
	case 0x01:
		c.ORA(info, operand, pageCrossed)
	case 0x05:
		c.ORA(info, operand, pageCrossed)
	case 0x06:
		c.ASL(info, address, pageCrossed)
	case 0x08:
		c.PHP(info, pageCrossed)
	case 0x09:
		c.ORA(info, operand, pageCrossed)
	case 0x0A:
		c.ASL(info, address, pageCrossed)
	case 0x0D:
		c.ORA(info, operand, pageCrossed)
	case 0x0E:
		c.ASL(info, address, pageCrossed)
	case 0x10:
		c.BPL(info, pageCrossed)
	case 0x11:
		c.ORA(info, operand, pageCrossed)
	case 0x15:
		c.ORA(info, operand, pageCrossed)
	case 0x16:
		c.ASL(info, address, pageCrossed)
	case 0x18:
		c.CLC(info, pageCrossed)
	case 0x19:
		c.ORA(info, operand, pageCrossed)
	case 0x1D:
		c.ORA(info, operand, pageCrossed)
	case 0x1E:
		c.ASL(info, address, pageCrossed)
	case 0x20:
		c.JSR(info, pageCrossed)
	case 0x21:
		c.AND(info, operand, pageCrossed)
	case 0x24:
		c.BIT(info, operand, pageCrossed)
	case 0x25:
		c.AND(info, operand, pageCrossed)
	case 0x26:
		c.ROL(info, address, pageCrossed)
	case 0x28:
		c.PLP(info, pageCrossed)
	case 0x29:
		c.AND(info, operand, pageCrossed)
	case 0x2A:
		c.ROL(info, address, pageCrossed)
	case 0x2C:
		c.BIT(info, operand, pageCrossed)
	case 0x2D:
		c.AND(info, operand, pageCrossed)
	case 0x2E:
		c.ROL(info, address, pageCrossed)
	case 0x30:
		c.BMI(info, pageCrossed)
	case 0x31:
		c.AND(info, operand, pageCrossed)
	case 0x35:
		c.AND(info, operand, pageCrossed)
	case 0x36:
		c.ROL(info, address, pageCrossed)
	case 0x38:
		c.SEC(info, pageCrossed)
	case 0x39:
		c.AND(info, operand, pageCrossed)
	case 0x3D:
		c.AND(info, operand, pageCrossed)
	case 0x3E:
		c.ROL(info, address, pageCrossed)
	case 0x40:
		c.RTI(info, pageCrossed)
	case 0x41:
		c.EOR(info, operand, pageCrossed)
	case 0x45:
		c.EOR(info, operand, pageCrossed)
	case 0x46:
		c.LSR(info, address, pageCrossed)
	case 0x48:
		c.PHA(info, pageCrossed)
	case 0x49:
		c.EOR(info, operand, pageCrossed)
	case 0x4A:
		c.LSR(info, address, pageCrossed)
	case 0x4C:
		c.JMP(info, pageCrossed)
	case 0x4D:
		c.EOR(info, operand, pageCrossed)
	case 0x4E:
		c.LSR(info, address, pageCrossed)
	case 0x50:
		c.BVC(info, pageCrossed)
	case 0x51:
		c.EOR(info, operand, pageCrossed)
	case 0x55:
		c.EOR(info, operand, pageCrossed)
	case 0x56:
		c.LSR(info, address, pageCrossed)
	case 0x58:
		c.CLI(info, pageCrossed)
	case 0x59:
		c.EOR(info, operand, pageCrossed)
	case 0x5D:
		c.EOR(info, operand, pageCrossed)
	case 0x5E:
		c.LSR(info, address, pageCrossed)
	case 0x60:
		c.RTS(info, pageCrossed)
	case 0x61:
		c.ADC(info, operand, pageCrossed)
	case 0x65:
		c.ADC(info, operand, pageCrossed)
	case 0x66:
		c.ROR(info, address, pageCrossed)
	case 0x68:
		c.PLA(info, pageCrossed)
	case 0x69:
		c.ADC(info, operand, pageCrossed)
	case 0x6A:
		c.ROR(info, address, pageCrossed)
	case 0x6C:
		c.JMP(info, pageCrossed)
	case 0x6D:
		c.ADC(info, operand, pageCrossed)
	case 0x6E:
		c.ROR(info, address, pageCrossed)
	case 0x70:
		c.BVS(info, pageCrossed)
	case 0x71:
		c.ADC(info, operand, pageCrossed)
	case 0x75:
		c.ADC(info, operand, pageCrossed)
	case 0x76:
		c.ROR(info, address, pageCrossed)
	case 0x78:
		c.SEI(info, pageCrossed)
	case 0x79:
		c.ADC(info, operand, pageCrossed)
	case 0x7D:
		c.ADC(info, operand, pageCrossed)
	case 0x7E:
		c.ROR(info, address, pageCrossed)
	case 0x81:
		c.STA(info, address, pageCrossed)
	case 0x84:
		c.STY(info, address, pageCrossed)
	case 0x85:
		c.STA(info, address, pageCrossed)
	case 0x86:
		c.STX(info, address, pageCrossed)
	case 0x88:
		c.DEY(info, pageCrossed)
	case 0x8A:
		c.TXA(info, pageCrossed)
	case 0x8C:
		c.STY(info, address, pageCrossed)
	case 0x8D:
		c.STA(info, address, pageCrossed)
	case 0x8E:
		c.STX(info, address, pageCrossed)
	case 0x90:
		c.BCC(info, pageCrossed)
	case 0x91:
		c.STA(info, address, pageCrossed)
	case 0x94:
		c.STY(info, address, pageCrossed)
	case 0x95:
		c.STA(info, address, pageCrossed)
	case 0x96:
		c.STX(info, address, pageCrossed)
	case 0x98:
		c.TYA(info, pageCrossed)
	case 0x99:
		c.STA(info, address, pageCrossed)
	case 0x9A:
		c.TXS(info, pageCrossed)
	case 0x9D:
		c.STA(info, address, pageCrossed)
	case 0xA0:
		c.LDY(info, operand, pageCrossed)
	case 0xA1:
		c.LDA(info, operand, pageCrossed)
	case 0xA2:
		c.LDX(info, operand, pageCrossed)
	case 0xA4:
		c.LDY(info, operand, pageCrossed)
	case 0xA5:
		c.LDA(info, operand, pageCrossed)
	case 0xA6:
		c.LDX(info, operand, pageCrossed)
	case 0xA8:
		c.TAY(info, pageCrossed)
	case 0xA9:
		c.LDA(info, operand, pageCrossed)
	case 0xAA:
		c.TAX(info, pageCrossed)
	case 0xAC:
		c.LDY(info, operand, pageCrossed)
	case 0xAD:
		c.LDA(info, operand, pageCrossed)
	case 0xAE:
		c.LDX(info, operand, pageCrossed)
	case 0xB0:
		c.BCS(info, pageCrossed)
	case 0xB1:
		c.LDA(info, operand, pageCrossed)
	case 0xB4:
		c.LDY(info, operand, pageCrossed)
	case 0xB5:
		c.LDA(info, operand, pageCrossed)
	case 0xB6:
		c.LDX(info, operand, pageCrossed)
	case 0xB8:
		c.CLV(info, pageCrossed)
	case 0xB9:
		c.LDA(info, operand, pageCrossed)
	case 0xBA:
		c.TSX(info, pageCrossed)
	case 0xBC:
		c.LDY(info, operand, pageCrossed)
	case 0xBD:
		c.LDA(info, operand, pageCrossed)
	case 0xBE:
		c.LDX(info, operand, pageCrossed)
	case 0xC0:
		c.CPY(info, operand, pageCrossed)
	case 0xC1:
		c.CMP(info, operand, pageCrossed)
	case 0xC4:
		c.CPY(info, operand, pageCrossed)
	case 0xC5:
		c.CMP(info, operand, pageCrossed)
	case 0xC6:
		c.DEC(info, address, pageCrossed)
	case 0xC8:
		c.INY(info, pageCrossed)
	case 0xC9:
		c.CMP(info, operand, pageCrossed)
	case 0xCA:
		c.DEX(info, pageCrossed)
	case 0xCC:
		c.CPY(info, operand, pageCrossed)
	case 0xCD:
		c.CMP(info, operand, pageCrossed)
	case 0xCE:
		c.DEC(info, address, pageCrossed)
	case 0xD0:
		c.BNE(info, pageCrossed)
	case 0xD1:
		c.CMP(info, operand, pageCrossed)
	case 0xD5:
		c.CMP(info, operand, pageCrossed)
	case 0xD6:
		c.DEC(info, address, pageCrossed)
	case 0xD8:
		c.CLD(info, pageCrossed)
	case 0xD9:
		c.CMP(info, operand, pageCrossed)
	case 0xDD:
		c.CMP(info, operand, pageCrossed)
	case 0xDE:
		c.DEC(info, address, pageCrossed)
	case 0xE0:
		c.CPX(info, operand, pageCrossed)
	case 0xE1:
		c.SBC(info, operand, pageCrossed)
	case 0xE4:
		c.CPX(info, operand, pageCrossed)
	case 0xE5:
		c.SBC(info, operand, pageCrossed)
	case 0xE6:
		c.INC(info, address, pageCrossed)
	case 0xE8:
		c.INX(info, pageCrossed)
	case 0xE9:
		c.SBC(info, operand, pageCrossed)
	case 0xEA:
		c.NOP(info, pageCrossed)
	case 0xEC:
		c.CPX(info, operand, pageCrossed)
	case 0xED:
		c.SBC(info, operand, pageCrossed)
	case 0xEE:
		c.INC(info, address, pageCrossed)
	case 0xF0:
		c.BEQ(info, pageCrossed)
	case 0xF1:
		c.SBC(info, operand, pageCrossed)
	case 0xF5:
		c.SBC(info, operand, pageCrossed)
	case 0xF6:
		c.INC(info, address, pageCrossed)
	case 0xF8:
		c.SED(info, pageCrossed)
	case 0xF9:
		c.SBC(info, operand, pageCrossed)
	case 0xFD:
		c.SBC(info, operand, pageCrossed)
	case 0xFE:
		c.INC(info, address, pageCrossed)
	default:
		fmt.Printf("Unknown opcode: %X\n", opcode)
	}

}

func (c *CPU) clock() {
	intialCycles := c.Cycle
	c.executeOpcode()
	return c.Cycle - intialCycles
}

func (c *CPU) reset() {
	c.PC = c.Read16(RESET_ADDR)
	c.SP = STACK_TOP
	c.P |= U
	c.Cycle = 7
}

func (c *CPU) interrupt() {
	if !c.getFlag(I) {
		c.push16(c.PC)
		c.pushStatus()
		c.setFLag(I, true)
		c.PC = c.Read16(IRQ_ADDR)
		c.Cycle = 7
	}
}

func (c *CPU) pushStatus() {
	c.push(c.P | B | U)
}

func (c *CPU) push(value uint8) {
	c.Write(STACK_START | uint16(c.SP) , value)
	c.SP--
}

func (c *CPU) push16(value uint16) {
	c.push(uint8(value >> 8))
	c.push(uint8(value & 0xFF))
}

func NewCPU() *CPU {
	cpu := &CPU{
		SP:  0xFD,
		P: 0x24,
	}
	return cpu
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