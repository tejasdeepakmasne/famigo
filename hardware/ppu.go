package hardware

import "fmt"

type PPU struct {
	VRAM    []uint8
	SPR_RAM []uint8

	//registers
	PCR1  uint8 //PPU control register 1 $2000
	PCR2  uint8 //PPU control register 2 $2001
	PSR   uint8 //PPU status register
	SRAR  uint8 //2003h - SPR-RAM Address Register
	SRDR  uint8 //2004h - SPR-RAM Data Register
	PBSO  uint8 //2005h - PPU Background Scrolling Offset
	VRAR  uint8 //2006h - VRAM Address Register
	VRDR  uint8 //2007h - VRAM Read/Write Data Register (RW)
	SPDMA uint8 //4014h - SPR-RAM DMA Register

	cycle, scanline uint16
}

func NewPPU() *PPU {
	ppu := &PPU{}
	ppu.VRAM = make([]uint8, 0x4000)
	ppu.SPR_RAM = make([]uint8, 0x100)
	return ppu
}

func (p *PPU) Step(cycles uint16) {
	for i := uint16(0); i < cycles; i++ {
		p.cycle++
		if p.cycle == 341 {
			p.cycle = 0
			p.scanline++
			if p.scanline == 262 {
				p.scanline = 0
				p.PSR |= 0b10000000 //Set VBlank flag
			}

		}
	}
}

func (p * PPU) Read(address uint16) uint8 {
	switch {
		case address < 0x2000:
			return p.VRAM[address]
		case address < 0x3F00:
			return p.VRAM[address % 0x0800 + 0x2000]
		case address < 0x4000:
			return p.VRAM[address % 0x20 + 0x3F00]
		default:
			return 0
	}
}

func (p *PPU) Write(address uint16, value uint8) {
	switch {
		case address < 0x2000:
			p.VRAM[address] = value
		case address < 0x3F00:
			p.VRAM[address % 0x0800 + 0x2000] = value
		case address < 0x4000:
			p.VRAM[address % 0x20 + 0x3F00] = value
	}
}

func main() {
	ppu := NewPPU()
	fmt.Println("Initialized PPU: ", ppu)
}
