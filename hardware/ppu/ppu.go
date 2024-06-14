package ppu

import "github.com/tejasdeepakmasne/famigo/hardware/memory"

type PPU struct {
	//memory
	Memory  memory.Memory
	VRAM    []uint8
	SPR_RAM []uint8

	//registers
	PCR1 uint8 //PPU control register 1 0x2000
	PCR2 uint8 //PPU control register 2 0x2001
	PSR  uint8 //PPU status Register 0x2002
	SRAR uint8 //SPR-RAM address register 0x2003
	SRDR uint8 //SPR-RAM data register 0x2004
	DMAR uint8 //sprite DMR register 0x4014
	BSO  uint8 //PPU Background Scrolling Offset 0x2005
	VRAR uint8 //VRAM address register 0x2006
	VRDR uint8 //VRAM data register 0x2007
}

func (p *PPU) ReadRegister(address uint16) uint8 {
	switch address {
	case 0x2002:
		return p.ReadPSR()
	default:
		return 0
	}
}

func (p *PPU) ReadPSR() uint8 {
	res := p.PSR & 0x1F
	return res
}
