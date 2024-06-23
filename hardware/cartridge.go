package hardware

type Cartridge struct {
	PRG_ROM []uint8
	CHR_ROM []uint8
	SRAM    []uint8
	Mapper  uint8
	Mirror  uint8
	Battery uint8
}
