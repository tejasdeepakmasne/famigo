package hardware

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
}
